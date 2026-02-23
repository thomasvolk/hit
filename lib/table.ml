module TokenMap = Map.Make (Text.Token)

module DocumentTable = struct
  module DocumentTableId = struct
    let prefix = "dtb"
  end

  module Id = Reference.Make (DocumentTableId)

  type t = {
    id : Id.t;
    map :
      (Text.TokenEntry.Flags.t * Text.Token.Pos.t list) Document.DocumentMap.t;
  }

  let id dt = dt.id
  let empty id = { id; map = Document.DocumentMap.empty }

  let add r (flags, pl) dt =
    { id = dt.id; map = Document.DocumentMap.add r (flags, pl) dt.map }

  let get k dt = Document.DocumentMap.find_opt k dt
  let all dt = Document.DocumentMap.to_list dt.map
  let filter p dt = { dt with map = Document.DocumentMap.filter p dt.map }
  let size dt = Document.DocumentMap.cardinal dt.map

  let merge dt dt' =
    {
      id = dt.id;
      map =
        Document.DocumentMap.union (fun _key v1 _v2 -> Some v1) dt.map dt'.map;
    }
end

module DocumentTableMap = Map.Make (DocumentTable.Id)

module TokenTable = struct
  type t = DocumentTable.Id.t TokenMap.t

  let add token dt_id tt = TokenMap.add token dt_id tt
  let get token tt = TokenMap.find_opt token tt
  let remove token tt = TokenMap.remove token tt

  let find_all predicate tt =
    TokenMap.to_list tt |> List.filter (fun (k, _) -> predicate k)

  let to_list tt = TokenMap.to_list tt
  let empty = TokenMap.empty
  let size tt = TokenMap.cardinal tt
  let merge tt tt' = TokenMap.union (fun _key v1 _v2 -> Some v1) tt tt'
end

module DocumentIdSet = Set.Make(Document.Id)

module DocumentRegister = struct
  type t = DocumentIdSet.t
  let empty = DocumentIdSet.empty
  let add d t = DocumentIdSet.add d t
  let remove d t = DocumentIdSet.remove d t
  let contains d t = DocumentIdSet.exists (fun a -> a = d) t
  let size t = DocumentIdSet.cardinal t
  let to_list t = DocumentIdSet.to_list t
end
