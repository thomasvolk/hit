module DocumentTable = struct
  module Id = struct
    include Reference
  end

  exception InvalidData of string

  module DocMap = Map.Make(Document.Id)

  type t = Token.Pos.t list DocMap.t

  let empty = DocMap.empty

  let add r pl t =
      if List.length pl > 0 then
        DocMap.add r pl t
      else
        raise (InvalidData "position list is empty")

  let size t = DocMap.cardinal t
end


module TokenTable = struct
  module TokenMap = Map.Make(Token)

  type t = DocumentTable.Id.t TokenMap.t

  let add k r t = TokenMap.add k r t

  let get k t = TokenMap.find_opt k t

  let empty = TokenMap.empty

  let size t = TokenMap.cardinal t
end

