
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


module TermIndex = struct

  module EntryMap = Map.Make(Ref)

  type t = {
    term: Term.t;
    entries: Term.Pos.t list EntryMap.t;
  }

  let create term = {
    term = term;
    entries = EntryMap.empty
  }

  let term t = t.term

  let ref t = Ref.create t.term

  let add r pl t =
      if List.length pl > 0 then
        { term = t.term; entries = EntryMap.add r pl t.entries }
      else
        raise (InvalidData "position list is empty")

  let size t = EntryMap.cardinal t.entries

end
