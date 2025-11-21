module Checksum : sig
  type t = string

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val create : string -> string
end

module DocumentId : sig
  val prefix : string
end

module Id : sig
  type t = string * string

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val create : string -> t
  val prefix : t -> string
  val hash : t -> string
  val to_string : t -> string
  val of_string : string -> t
  val compare : t -> t -> int
end

module Meta : sig
  type t = { source : string; path : string; checksum : Checksum.t }

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val make_reference : string -> string -> string
  val create : string -> string -> Checksum.t -> t
  val path : t -> string
  val name : t -> string
  val directory : t -> string
  val extension : t -> string
  val title : t -> string
  val source : t -> string
  val reference : t -> string
  val id : t -> Id.t
  val checksum : t -> Checksum.t
end

type t = { meta : Meta.t; content : string }

val create : Meta.t -> string -> t
val from_source : string -> string -> string -> t
val content : t -> string
val meta : t -> Meta.t
val id : t -> Id.t
val checksum : t -> Checksum.t

module DocumentMap : Map.S with type key = Id.t
