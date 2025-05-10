
(* A Term is a word that can be searched *)

type t = string

module Pos : sig
  type t = int

  val to_int : t -> int
end

val to_string : t -> string

val compare : t -> t -> int

