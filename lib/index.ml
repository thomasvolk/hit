type t = {
  tokens: Model.TokenTable.t
}

module Make (Storage : Io.StorageInstance) = struct
  let create = { tokens=Storage.Impl.load_token_table Storage.t }

  let rec add_entries idx doc_id = function
    | [] -> idx
    | entry :: rest -> 
      let token = Analyzer.Entry.token entry in
      let dti = Model.DocumentTable.Id.create token in
      let dt = Storage.Impl.load_doc_table dti Storage.t in
      let dt' = Model.DocumentTable.add doc_id (Analyzer.Entry.positions entry) dt in
      Storage.Impl.save_doc_table dt' Storage.t;
      let idx' = Model.TokenTable.add token dti idx in
      add_entries idx' doc_id rest

  let add_doc d idx = 
    let tt = idx.tokens in
    Storage.Impl.save_doc d Storage.t;
    let did = Model.Document.id d in
    let entries = Analyzer.Parser.parse (Model.Document.content d) in
    let tt' = add_entries tt did entries in
    Storage.Impl.save_token_table tt' Storage.t;
    { tokens=tt'}

  let find_docs tokens idx =
    let get_docs token =
      match Model.TokenTable.get token idx.tokens with
      | None -> []
      | Some dti ->
        let dt = Storage.Impl.load_doc_table dti Storage.t in
        Model.DocumentTable.to_doc_list dt
    in
    List.flatten (List.map get_docs tokens) |> List.sort_uniq compare

end
