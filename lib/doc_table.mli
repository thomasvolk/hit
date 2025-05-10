
exception InvalidData of string

(* This is a index entry of one term. It includes all references and
   positions of the term in the documents.
*)

module DocMap : Map.S with type key = Doc.ref

type t = private {
  docs: Term.Pos.t list DocMap.t;
}

val create : t

val add : Doc.ref -> Term.Pos.t list -> t -> t

val size : t -> int

