module type PrefixType = sig
  val prefix : string
end

val hash : string -> string

module type IdType = sig
  type t = string * string

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val of_hash : string -> t
  val create : string -> t
  val prefix : string
  val hash : t -> string
  val to_string : t -> string
  val of_string : string -> t
  val compare : t -> t -> int
end

module Make : (_ : PrefixType) -> IdType
