open Table

type t = {
  token_table : TokenTable.t;
  doc_tables : DocumentTable.t DocumentTableMap.t;
  documents : Document.t DocumentMap.t;
  config : Config.IndexConfig.t;
}

module SearchResult = struct
  type t = { doc_id : Document.Id.t; token_entries : Text.TokenEntry.t list }

  let create d tel = { doc_id = d; token_entries = tel }
  let from_tuple (d, tel) = create d tel
  let doc_id sr = sr.doc_id
  let token_entries sr = sr.token_entries

  let best_matches sr =
    let rec loop r c = function
      | [] -> r
      | n :: rest -> (
          match Text.TokenEntry.closest_distance c n with
          | None -> loop r n rest
          | Some d -> loop (r @ [ d ]) n rest)
    in
    let te = sr.token_entries |> List.filter Text.TokenEntry.has_positions in
    match te with
      | [] -> []
      | c :: rest -> loop [] c rest

  let score cfg sr =
    let c =
      List.map Text.TokenEntry.count sr.token_entries
      |> List.mapi (fun i c -> c + (i * Config.IndexConfig.max_token_count cfg))
      |> List.fold_left ( * ) 1 |> Float.of_int
    in
    let f =
      List.map Float.of_int
        (best_matches sr |> List.map Text.Token.Distance.distance)
      |> List.map (fun d -> 1. /. (1. +. d))
      |> List.fold_left ( +. ) 0.
    in
    (1. +. c) *. (1. +. f) |> Int.of_float

  let compare cfg a b = score cfg b - score cfg a
end

module Make (Storage : Io.StorageInstance) = struct
  let load () =
    {
      token_table = Storage.Impl.load_token_table Storage.t;
      doc_tables = DocumentTableMap.empty;
      documents = DocumentMap.empty;
      config = Storage.Impl.load_index_config Storage.t;
    }

  let init () =
    if not (Storage.Impl.index_config_exists Storage.t) then
      let c = Config.IndexConfig.create () in
      Storage.Impl.save_index_config c Storage.t

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
            config = idx.config;
          }
        in
        add_entries idx' doc_id rest

  let add_doc d idx =
    let did = Document.id d in
    let entries = Text.Parser.parse (Document.content d) in
    Logs.info (fun m ->
        m "Document: %s - tokens found: %d"
          (Document.Meta.source (Document.meta d))
          (List.length entries));
    let idx' =
      {
        token_table = idx.token_table;
        doc_tables = idx.doc_tables;
        documents = DocumentMap.add did d idx.documents;
        config = idx.config;
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
    |> List.map SearchResult.from_tuple
    |> List.sort (SearchResult.compare idx.config)

  let get_doc did = Storage.Impl.load_doc did Storage.t

  let flush ?(clear_cache = true) ?(force = false) idx =
    Logs.info (fun m -> m "Flush index");
    Storage.Impl.with_lock ~force
      (fun () ->
        let current_tt = Storage.Impl.load_token_table Storage.t in
        Logs.info (fun m -> m "Merge token_table");
        let merged_tt = TokenTable.merge idx.token_table current_tt in
        Logs.info (fun m -> m "Write token_table");
        Storage.Impl.save_token_table merged_tt Storage.t;
        DocumentTableMap.iter
          (fun _ dt ->
            let current_dt =
              Storage.Impl.load_doc_table (DocumentTable.id dt) Storage.t
            in
            Logs.info (fun m ->
                m "Merge document table %s"
                  (DocumentTable.Id.to_string (DocumentTable.id dt)));
            let merged_dt = DocumentTable.merge dt current_dt in
            Logs.info (fun m ->
                m "Write document table %s"
                  (DocumentTable.Id.to_string (DocumentTable.id dt)));
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
            config = idx.config;
          }
        else idx)
      Storage.t
end
