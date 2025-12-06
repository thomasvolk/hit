module type PrefixType = sig
  (** The prefix type defines a unique prefix for the identifier type. *)

  val prefix : string
end

val hash : string -> string
(** [hash s] computes the hash of the given string [s] *)

module type IdType = sig
  (** This module type defines the interface for an identifier type. *)

  type t = string
  (** The identifier type is represented as a string. *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  (** [t_of_sexp sexp] converts a S-expression [sexp] into an identifier [t] *)

  val sexp_of_t : t -> Sexplib.Sexp.t
  (** [sexp_of_t id] converts an identifier [t] into a S-expression *)

  val of_hash : string -> t
  (** [of_hash hash] creates an identifier [t] from the given [hash] string. *)

  val create : string -> t
  (** [create s] creates an identifier [t] from the given string [s]. *)

  val prefix : string
  (** The unique prefix associated with this identifier type. *)

  val hash : t -> string
  (** [hash id] retrieves the hash string from the identifier [id]. *)

  val to_string : t -> string
  (** [to_string id] converts the identifier [id] to its string representation.
  *)

  val of_string : string -> t
  (** [of_string s] creates an identifier [t] from the given string [s]. *)

  val compare : t -> t -> int
  (** [compare id1 id2] compares two identifiers [id1] and [id2]. *)
end

module Make : (_ : PrefixType) -> IdType
(** This functor creates an identifier module with a unique prefix. *)
