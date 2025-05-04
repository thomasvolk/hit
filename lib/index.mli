
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

end


module Entry : sig
  (* This is a index entry of one term. It includes all references and
     positions of the term in the documents.
  *)

  module DocMap : Map.S with type key = Doc.ref

  type t = private {
    term: Term.t;
    entries: Term.Pos.t list DocMap.t;
  }

  val term : t -> Term.t

  val ref : t -> Ref.t

  val create : Term.t -> t

  val add : Doc.ref -> Term.Pos.t list -> t -> t

  val size : t -> int

end
