type t = {
  tokens: Model.TokenTable.t
}

module Make (Storage : Io.StorageInstance) = struct
  let create = { tokens=Storage.Impl.load_token_table Storage.t }

  let rec add_entries t doc_id = function
    | [] -> t
    | entry :: rest -> 
      let token = Analyzer.Entry.token entry in
      let dti = Model.DocumentTable.Id.create token in
      let dt = Storage.Impl.load_doc_table dti Storage.t in
      let dt' = Model.DocumentTable.add doc_id (Analyzer.Entry.positions entry) dt in
      Storage.Impl.save_doc_table dt' Storage.t;
      let t' = Model.TokenTable.add token dti t in
      add_entries t' doc_id rest

  let update_document d t = 
    Storage.Impl.save_doc d Storage.t;
    let did = Model.Document.id d in
    let entries = Analyzer.Parser.parse (Model.Document.content d) in
    let t' = add_entries t did entries in
    Storage.Impl.save_token_table t' Storage.t;
    t'

end
