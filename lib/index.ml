
module TokenTable = struct
  module TermMap = Map.Make(Token)

  type t = Ref.t TermMap.t

  let add k r t = TermMap.add k r t

  let get k t = TermMap.find_opt k t

  let empty = TermMap.empty

  let size t = TermMap.cardinal t
end


module DocumentTable = struct
  exception InvalidData of string

  module DocMap = Map.Make(Ref)

  type t = Token.Pos.t list DocMap.t

  let empty = DocMap.empty

  let add r pl t =
      if List.length pl > 0 then
        DocMap.add r pl t
      else
        raise (InvalidData "position list is empty")

  let size t = DocMap.cardinal t
end
