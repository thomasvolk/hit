open Table

type t = {
  token_table : TokenTable.t;
  doc_tables : DocumentTable.t DocumentTableMap.t;
  documents : Document.t DocumentMap.t;
}

module SearchResult = struct
  type t = { doc : Document.Id.t }
end

module Make (Storage : Io.StorageInstance) = struct
  let create =
    {
      token_table = Storage.Impl.load_token_table Storage.t;
      doc_tables = DocumentTableMap.empty;
      documents = DocumentMap.empty;
    }

  let get_doc_table dt_id idx =
    match DocumentTableMap.find_opt dt_id idx.doc_tables with
    | Some dt -> dt
    | None -> Storage.Impl.load_doc_table dt_id Storage.t

  let rec add_entries idx doc_id = function
    | [] -> idx
    | entry :: rest ->
        let token = Text.TokenEntry.token entry in
        let dt_id = DocumentTable.Id.create token in
        let dt = get_doc_table dt_id idx in
        let dt' =
          DocumentTable.add doc_id (Text.TokenEntry.positions entry) dt
        in
        let tt' = TokenTable.add token dt_id idx.token_table in
        let idx' =
          {
            token_table = tt';
            doc_tables = DocumentTableMap.add dt_id dt' idx.doc_tables;
            documents = idx.documents;
          }
        in
        add_entries idx' doc_id rest

  let add_doc d idx =
    let did = Document.id d in
    let entries = Text.Parser.parse (Document.content d) in
    let idx' =
      {
        token_table = idx.token_table;
        doc_tables = idx.doc_tables;
        documents = DocumentMap.add did d idx.documents;
      }
    in
    add_entries idx' did entries

  let find_docs tokens idx =
    let get_docs token =
      match TokenTable.get token idx.token_table with
      | None -> []
      | Some dti ->
          let dt = get_doc_table dti idx in
          DocumentTable.all dt
          |> List.map (fun (did, pl) ->
                 (did, [ Text.TokenEntry.create token pl ]))
    in
    let rec merge r = function
      | [] -> r
      | (did, el) :: tl ->
          let r' =
            match DocumentMap.find_opt did r with
            | None -> DocumentMap.add did el r
            | Some cel -> DocumentMap.add did (cel @ el) r
          in
          merge r' tl
    in
    List.flatten (List.map get_docs tokens)
    |> merge DocumentMap.empty |> DocumentMap.to_list

  let get_doc did = Storage.Impl.load_doc did Storage.t

  let flush ?(clear_cache = true) ?(force = false) idx =
    Storage.Impl.with_lock ~force
      (fun () ->
        let current_tt = Storage.Impl.load_token_table Storage.t in
        let merged_tt = TokenTable.merge idx.token_table current_tt in
        Storage.Impl.save_token_table merged_tt Storage.t;
        DocumentTableMap.iter
          (fun _ dt ->
            let current_dt =
              Storage.Impl.load_doc_table (DocumentTable.id dt) Storage.t
            in
            let merged_dt = DocumentTable.merge dt current_dt in
            Storage.Impl.save_doc_table merged_dt Storage.t)
          idx.doc_tables;
        DocumentMap.iter
          (fun _ d -> Storage.Impl.save_doc d Storage.t)
          idx.documents;
        if clear_cache then
          {
            token_table = idx.token_table;
            doc_tables = DocumentTableMap.empty;
            documents = DocumentMap.empty;
          }
        else idx)
      Storage.t
end
