
exception InvalidData of string

module DocMap = Map.Make(Ref)

type t = {
  docs: Term.Pos.t list DocMap.t;
}

let create = {
  docs = DocMap.empty
}

let add r pl t =
    if List.length pl > 0 then
      { docs = DocMap.add r pl t.docs }
    else
      raise (InvalidData "position list is empty")

let size t = DocMap.cardinal t.docs

