
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

  module Entry = struct

    type t = Ref.t * int list

    exception InvalidEntry of string

    let ref t = fst t

    let positions t = snd t

    let create d pl = 
      if List.length pl > 0 then
        (d, pl)
      else
        raise (InvalidEntry "position list is empty")
  end

  type t = {
    term: Term.t;
    entries: Entry.t EntryMap.t;
  }

  let create term = {
    term = term;
    entries = EntryMap.empty
  }

  let term t = t.term

  let ref t = Ref.create t.term

  let add e t = { term = t.term; entries = EntryMap.add (Entry.ref e) e t.entries }

  let size t = EntryMap.cardinal t.entries
end
