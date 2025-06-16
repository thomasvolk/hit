type t = {
  token_table : Model.TokenTable.t;
  doc_tables : Model.DocumentTable.t Model.DocumentTableMap.t;
  documents : Model.Document.t Model.DocumentMap.t;
}

module Make (Storage : Io.StorageInstance) = struct
  let create =
    {
      token_table = Storage.Impl.load_token_table Storage.t;
      doc_tables = Model.DocumentTableMap.empty;
      documents = Model.DocumentMap.empty;
    }

  let get_doc_table dt_id idx =
    match Model.DocumentTableMap.find_opt dt_id idx.doc_tables with
    | Some dt -> dt
    | None -> Storage.Impl.load_doc_table dt_id Storage.t

  let rec add_entries idx doc_id = function
    | [] -> idx
    | entry :: rest ->
        let token = Analyzer.Entry.token entry in
        let dt_id = Model.DocumentTable.Id.create token in
        let dt = get_doc_table dt_id idx in
        let dt' =
          Model.DocumentTable.add doc_id (Analyzer.Entry.positions entry) dt
        in
        let tt' = Model.TokenTable.add token dt_id idx.token_table in
        let idx' =
          {
            token_table = tt';
            doc_tables = Model.DocumentTableMap.add dt_id dt' idx.doc_tables;
            documents = idx.documents;
          }
        in
        add_entries idx' doc_id rest

  let add_doc d idx =
    let did = Model.Document.id d in
    let entries = Analyzer.Parser.parse (Model.Document.content d) in
    let idx' =
      {
        token_table = idx.token_table;
        doc_tables = idx.doc_tables;
        documents = Model.DocumentMap.add did d idx.documents;
      }
    in
    add_entries idx' did entries

  let find_docs tokens idx =
    let get_docs token =
      match Model.TokenTable.get token idx.token_table with
      | None -> []
      | Some dti ->
          let dt = get_doc_table dti idx in
          Model.DocumentTable.to_doc_list dt
    in
    List.flatten (List.map get_docs tokens) |> List.sort_uniq compare

  let get_doc did = Storage.Impl.load_doc did Storage.t

  let flush ?(clear_cache = true) idx =
    let current_tt = Storage.Impl.load_token_table Storage.t in 
    let merged_tt = Model.TokenTable.merge idx.token_table current_tt in
    Storage.Impl.save_token_table merged_tt Storage.t;
    Model.DocumentTableMap.iter
      (fun _ dt ->
        let current_dt = Storage.Impl.load_doc_table (Model.DocumentTable.id dt) Storage.t in
        let merged_dt = Model.DocumentTable.merge dt current_dt in
        Storage.Impl.save_doc_table merged_dt Storage.t)
      idx.doc_tables;
    Model.DocumentMap.iter
      (fun _ d -> Storage.Impl.save_doc d Storage.t)
      idx.documents;
    if clear_cache then
      {
        token_table = idx.token_table;
        doc_tables = Model.DocumentTableMap.empty;
        documents = Model.DocumentMap.empty;
      }
    else idx
end
