
module Doc : sig

  type ref = Ref.t

end


module Term : sig
  (* A Term is a word that can be searched *)

  type t = string
  type pos = int

  val to_string : t -> string

end


module TermIndex : sig
  (* A TermIndex is an index of one term. It includes all references and
     positions of the term in the documents.
  *)

  module EntryMap : Map.S with type key = Doc.ref

  module Entry : sig

    exception InvalidEntry of string

    type t = private Doc.ref * Term.pos list

    val create : Doc.ref -> Term.pos list -> t
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
