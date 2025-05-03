
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

  let add e t = EntryMap.add (Entry.ref e) e t

  let size t = EntryMap.cardinal t
end
