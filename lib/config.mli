val default_separators : string

module IndexConfig : sig
  type t = {
    max_token_count : int;
    token_separators : string;
    min_token_length : int;
  }

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val create :
    ?max_token_count:int ->
    ?token_separators:string ->
    ?min_token_length:int ->
    unit ->
    t

  val max_token_count : t -> int
  val token_separators : t -> string
  val token_separators_seq : t -> char list
  val min_token_length : t -> int
end
