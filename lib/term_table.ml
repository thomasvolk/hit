
module TermMap = Map.Make(Term)

type t = Ref.t TermMap.t

