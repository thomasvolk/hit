(** Configuration module *)

module IndexConfig : sig
  (** The [IndexConfig] module defines the configuration settings for indexing
      text documents. It includes parameters such as maximum token count, token
      and minimum token length. *)

  type t = {
    max_token_count : int;
    min_token_length : int;
  }
  (** The [t] type represents the index configuration with the following fields:
      - [max_token_count]: This value limits the number of tokens which are able
        to push the score of a search result higher.
        tokens.
      - [min_token_length]: The minimum length a token must have to be indexed.
  *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  (** [t_of_sexp sexp] converts a S-expression [sexp] into an index
      configuration [t] *)

  val sexp_of_t : t -> Sexplib.Sexp.t
  (** [sexp_of_t config] converts an index configuration [t] into a S-expression
  *)

  val create :
    ?max_token_count:int ->
    ?min_token_length:int ->
    unit ->
    t
  (** [create ?max_token_count ?min_token_length ()] creates
      an index configuration [t] with the specified parameters. *)

  val max_token_count : t -> int
  val min_token_length : t -> int
end
