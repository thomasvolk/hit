
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

end


module Entry = struct

  module DocMap = Map.Make(Ref)

  type t = {
    term: Term.t;
    entries: Term.Pos.t list DocMap.t;
  }

  let create term = {
    term = term;
    entries = DocMap.empty
  }

  let term t = t.term

  let ref t = Ref.create t.term

  let add r pl t =
      if List.length pl > 0 then
        { term = t.term; entries = DocMap.add r pl t.entries }
      else
        raise (InvalidData "position list is empty")

  let size t = DocMap.cardinal t.entries

end
