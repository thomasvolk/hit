
exception InvalidData of string


module Doc : sig
  (* This is the index document *)

  type ref = Ref.t

end


module Term : sig
  (* A Term is a word that can be searched *)

  type t = string

  module Pos : sig
    type t = int

    val to_int : t -> int
  end

  val to_string : t -> string
  
  val compare : t -> t -> int

end


module DocRefTable : sig
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

end


module EntryMap : Map.S with type key = Term.t


type t = private DocRefTable.t EntryMap.t

val create : t

val add : Term.t -> DocRefTable.t -> t -> t
