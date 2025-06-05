type t = {
  tokens: Model.TokenTable.t
}

module Make (Storage : Io.StorageInstance) = struct
  let create = { tokens=Storage.Impl.load_token_table Storage.t }
end
