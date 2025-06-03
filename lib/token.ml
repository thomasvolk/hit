
(* A Term is a word that can be searched *)

type t = string

module Pos = struct
  type t = int

  let to_int t = t
end

let to_string t = t

let compare a b = String.compare a b

