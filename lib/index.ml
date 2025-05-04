
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

    type t = Term.Pos.t list

    exception InvalidEntry of string

    let positions t = t

    let create pl = 
      if List.length pl > 0 then
        pl
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

  let add r pl t = { term = t.term; entries = EntryMap.add r pl t.entries }

  let size t = EntryMap.cardinal t.entries

end
