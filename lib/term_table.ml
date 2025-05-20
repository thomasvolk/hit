
module TermMap = Map.Make(Term)

type t = Ref.t TermMap.t

let add k r t = TermMap.add k r t

let empty = TermMap.empty
