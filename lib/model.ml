open Sexplib.Std

module Document = struct
  module DocumentId = struct
    let prefix = "doc"
  end

  module Id = Reference.Make (DocumentId)

  module Meta = struct
    type t = { source : string; path : string } [@@deriving sexp]

    let create s p = { source = s; path = p }
    let path m = m.path
    let source m = m.source
    let id m = m.source ^ "::" ^ m.path
  end

  type t = { id : Id.t; meta : Meta.t; content : string }

  let create m c = { id = Id.create (Meta.id m); meta = m; content = c }
  let id d = d.id
  let content d = d.content
  let meta d = d.meta
end

module DocumentTable = struct
  module DocumentTableId = struct
    let prefix = "dtb"
  end

  module Id = Reference.Make (DocumentTableId)

  exception InvalidData of string

  module DocMap = Map.Make (Document.Id)

  type t = { id : Id.t; map : Token.Pos.t list DocMap.t }

  let id dt = dt.id
  let empty id = { id; map = DocMap.empty }

  let add r pl dt =
    if List.length pl > 0 then { id = dt.id; map = DocMap.add r pl dt.map }
    else raise (InvalidData "position list is empty")

  let get k dt = DocMap.find_opt k dt
  let all dt = DocMap.to_list dt.map
  let size dt = DocMap.cardinal dt.map
  let to_doc_list dt = DocMap.to_list dt.map |> List.map fst
end

module TokenTable = struct
  module TokenMap = Map.Make (Token)

  type t = DocumentTable.Id.t TokenMap.t

  let add token dt_id tt = TokenMap.add token dt_id tt
  let get token tt = TokenMap.find_opt token tt
  let empty = TokenMap.empty
  let size tt = TokenMap.cardinal tt
end
