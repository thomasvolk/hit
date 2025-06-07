open Sexplib.Std

module Document = struct
  module DocumentId = struct
    let prefix = "doc"
  end
  module Id = Reference.Make(DocumentId)

  module Meta = struct
    type t = {
      source: string;
      path: string;
    } [@@deriving sexp]

    let create s p = { source=s; path=p; }

    let path m = m.path

    let source m = m.source

    let id m = m.source ^ "::" ^ m.path
  end

  type t = {
    id: Id.t;
    meta: Meta.t;
    content: string;
  }

  let create m c = {
    id=(Id.create (Meta.id m));
    meta=m;
    content=c
  }

  let id d = d.id

  let content d = d.content

  let meta d = d.meta
end

module DocumentTable = struct
  module DocumentTableId = struct
    let prefix = "dtb"
  end
  module Id = Reference.Make(DocumentTableId)

  exception InvalidData of string

  module DocMap = Map.Make(Document.Id)

  type t = {
    id: Id.t;
    map: Token.Pos.t list DocMap.t
  }

  let id t = t.id

  let empty id = { id=id; map=DocMap.empty }

  let add r pl t =
      if List.length pl > 0 then
        { id=t.id; map=DocMap.add r pl t.map }
      else
        raise (InvalidData "position list is empty")

  let get k t = DocMap.find_opt k t

  let all t = DocMap.to_list t.map

  let size t = DocMap.cardinal t.map
end


module TokenTable = struct
  module TokenMap = Map.Make(Token)

  type t = DocumentTable.Id.t TokenMap.t

  let add k r t = TokenMap.add k r t

  let get k t = TokenMap.find_opt k t

  let empty = TokenMap.empty

  let size t = TokenMap.cardinal t
end
