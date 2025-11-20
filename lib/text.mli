module Token : sig
  type t = string

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val length : string -> int

  module Pos : sig
    type t = int

    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
    val to_int : 'a -> 'a
  end

  val to_string : 'a -> 'a
  val compare : String.t -> String.t -> int
end

module TokenPair : sig
  type t = (Token.t * int) * (Token.t * int)

  val create : 'a -> 'b -> 'c -> 'd -> ('a * 'b) * ('c * 'd)
  val st : 'a * 'b -> 'a
  val en : 'a * 'b -> 'b
  val distance_vec : ('a * int) * ('b * int) -> int
  val distance : ('a * int) * ('b * int) -> int
end

module TokenEntry : sig
  module Flags : sig
    type t = { title : bool; directory : bool; extension : bool; source : bool }

    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
    val empty : t
    val create : bool -> bool -> bool -> bool -> t
    val from_string : string -> t
    val to_string : t -> string
    val create_title : t
    val create_directory : t
    val create_extension : t
    val create_source : t
    val set_title : t -> t
    val set_directory : t -> t
    val set_extension : t -> t
    val set_source : t -> t
    val title : t -> bool
    val directory : t -> bool
    val extension : t -> bool
    val source : t -> bool
  end

  type t = { token : Token.t; positions : Token.Pos.t list; flags : Flags.t }

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val create : Token.t -> Token.Pos.t list -> Flags.t -> t
  val create_title : Token.t -> Token.Pos.t list -> t
  val create_directory : Token.t -> Token.Pos.t list -> t
  val create_extension : Token.t -> Token.Pos.t list -> t
  val create_source : Token.t -> Token.Pos.t list -> t
  val token : t -> Token.t
  val token_length : t -> int
  val positions : t -> Token.Pos.t list
  val count : t -> int
  val has_positions : t -> bool
  val add_position : Token.Pos.t -> t -> t
  val flags : t -> Flags.t
  val set_title : t -> t
  val set_directory : t -> t
  val set_extension : t -> t
  val set_source : t -> t
  val in_range : Token.Pos.t -> int -> t -> (Token.Pos.t * int) list

  val closest_distance :
    t -> t -> ((Token.t * Token.Pos.t) * (Token.t * Token.Pos.t)) option
end

val split_on_control_chars : string -> string list

module Parser : sig
  module TokenMap : Map.S with type key = Token.t

  val get_tokens :
    char list -> ?min_token_length:int -> string -> (string * int) list

  val parse :
    char list -> ?min_token_length:int -> Document.t -> TokenEntry.t list
end
