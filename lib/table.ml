open Sexplib.Std
module TokenMap = Map.Make (Text.Token)

module Document = struct
  module Checksum = struct 
    type t = string [@@deriving sexp]
    let create cnt = Digest.MD5.string cnt |> Digest.MD5.to_hex
  end
  module DocumentId = struct
    let prefix = "doc"
  end

  module Id = Reference.Make (DocumentId)

  module Meta = struct
    type t = { source : string; path : string; checksum: Checksum.t } [@@deriving sexp]

    let make_reference s p = s ^ "::" ^ p
    let create s p c = { source = s; path = p; checksum = c }
    let path m = m.path
    let source m = m.source
    let reference m = make_reference m.source m.path
    let id m = Id.create (reference m)
    let checksum m = m.checksum
  end

  type t = { meta : Meta.t; content : string }

  let create m c = { meta = m; content = c }

  let from_source s p c =
    let m = Meta.create s p (Checksum.create c) in
    { meta = m; content = c }
  let content d = d.content
  let meta d = d.meta
  let id d = Meta.id d.meta
  let checksum d = d.meta.checksum
end

module DocumentMap = Map.Make (Document.Id)

module DocumentTable = struct
  module DocumentTableId = struct
    let prefix = "dtb"
  end

  module Id = Reference.Make (DocumentTableId)

  exception InvalidData of string

  type t = { id : Id.t; map : Text.Token.Pos.t list DocumentMap.t }

  let id dt = dt.id
  let empty id = { id; map = DocumentMap.empty }

  let add r pl dt =
    if List.length pl > 0 then { id = dt.id; map = DocumentMap.add r pl dt.map }
    else raise (InvalidData "position list is empty")

  let get k dt = DocumentMap.find_opt k dt
  let all dt = DocumentMap.to_list dt.map
  let size dt = DocumentMap.cardinal dt.map

  let merge dt dt' =
    {
      id = dt.id;
      map = DocumentMap.union (fun _key v1 _v2 -> Some v1) dt.map dt'.map;
    }
end

module DocumentTableMap = Map.Make (DocumentTable.Id)

module TokenTable = struct
  type t = DocumentTable.Id.t TokenMap.t

  let add token dt_id tt = TokenMap.add token dt_id tt
  let get token tt = TokenMap.find_opt token tt
  let empty = TokenMap.empty
  let size tt = TokenMap.cardinal tt
  let merge tt tt' = TokenMap.union (fun _key v1 _v2 -> Some v1) tt tt'
end
