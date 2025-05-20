
exception InvalidData of string

module DocMap = Map.Make(Ref)

type t = Term.Pos.t list DocMap.t

let empty = DocMap.empty

let add r pl t =
    if List.length pl > 0 then
      DocMap.add r pl t
    else
      raise (InvalidData "position list is empty")

let size t = DocMap.cardinal t

