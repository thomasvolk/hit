
module Doc : sig

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


module TermIndex : sig
  (* A TermIndex is an index of one term. It includes all references and
     positions of the term in the documents.
  *)

  module EntryMap : Map.S with type key = Doc.ref

  module Entry : sig

    exception InvalidEntry of string

    type t = private Doc.ref * Term.Pos.t list

    val create : Doc.ref -> Term.Pos.t list -> t

    val ref : t -> Doc.ref

    val positions : t -> Term.Pos.t list
  end

  type t = {
    term: Term.t;
    entries: Entry.t EntryMap.t;
  }

  val term : t -> Term.t

  val ref : t -> Ref.t

  val create : Term.t -> t

  val add : Entry.t -> t -> t

  val size : t -> int

end
