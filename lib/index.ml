
exception InvalidData of string


module Doc = struct

  type ref = Ref.t

end


module Term = struct

  type t = string

  module Pos = struct
    type t = int

    let to_int t = t
  end

  let to_string t = t

  let compare a b = String.compare a b

end


module Entry = struct

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

end


module EntryMap = Map.Make(Term)

type t = Entry.t EntryMap.t

let create = EntryMap.empty

let add k v t = EntryMap.add k v t

