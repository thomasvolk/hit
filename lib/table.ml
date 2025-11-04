module TokenMap = Map.Make (Text.Token)

module DocumentTable = struct
  module DocumentTableId = struct
    let prefix = "dtb"
  end

  module Id = Reference.Make (DocumentTableId)

  exception InvalidData of string

  type t = { id : Id.t; map : Text.Token.Pos.t list Document.DocumentMap.t }

  let id dt = dt.id
  let empty id = { id; map = Document.DocumentMap.empty }

  let add r pl dt =
    if List.length pl > 0 then
      { id = dt.id; map = Document.DocumentMap.add r pl dt.map }
    else raise (InvalidData "position list is empty")

  let get k dt = Document.DocumentMap.find_opt k dt
  let all dt = Document.DocumentMap.to_list dt.map
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

  let find_all predicate tt =
    TokenMap.to_list tt |> List.filter (fun (k, _) -> predicate k)

  let empty = TokenMap.empty
  let size tt = TokenMap.cardinal tt
  let merge tt tt' = TokenMap.union (fun _key v1 _v2 -> Some v1) tt tt'
end
