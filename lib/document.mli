(** Representation of a document with metadata and content. *)

module Checksum : sig
  (** A module for computing and representing checksums of document content. *)

  type t = string
  (** The checksum type is represented as a string. *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  (** [t_of_sexp sexp] converts a S-expression [sexp] into a checksum [t] *)

  val sexp_of_t : t -> Sexplib.Sexp.t
  (** [sexp_of_t checksum] converts a checksum [t] into a S-expression *)

  val create : string -> string
  (** [create content] computes the checksum of the given [content] string. *)
end

module DocumentId : sig
  val prefix : string
end

module Id : Reference.IdType

module Meta : sig
  (** A module representing the metadata of a document. *)

  type t = { source : string; path : string; checksum : Checksum.t }
  (** The metadata type contains:
      - [source]: the source of the document (this can be a URL or a device)
      - [path]: the file path of the document
      - [checksum]: the checksum of the document content) *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val create : string -> string -> Checksum.t -> t
  val path : t -> string
  val name : t -> string
  val directory : t -> string
  val extension : t -> string
  val title : t -> string
  val source : t -> string

  val reference : t -> string
  (** [reference meta] creates a unique reference string for the document based
      on its source and path. *)

  val id : t -> Id.t
  val checksum : t -> Checksum.t
end

type t = { meta : Meta.t; content : string }
(** The document type contains:
    - [meta]: the metadata of the document
    - [content]: the actual content of the document as a string *)

val create : Meta.t -> string -> t

val from_source : string -> string -> string -> t
(** [from_source source path content] creates a document [t] from the given
    [source], [path], and [content], computing the checksum automatically. *)

val content : t -> string
val meta : t -> Meta.t
val id : t -> Id.t
val checksum : t -> Checksum.t

module DocumentMap : Map.S with type key = Id.t
