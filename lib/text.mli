(** Module for text tokenization and token entry management. *)

module Token : sig
  (** The [Token] module defines the type and operations for text tokens. *)

  type t = string
  (** The token type is represented as a string. *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  (** [t_of_sexp sexp] converts a S-expression [sexp] into a token [t]. *)

  val sexp_of_t : t -> Sexplib.Sexp.t
  (** [sexp_of_t token] converts a token [t] into a S-expression. *)

  val length : t -> int
  (** [length token] returns the length of the token [token]. *)

  module Pos : sig
    (** The [Pos] module defines the type and operations for token positions. *)

    type t = int
    (** The position type is represented as an integer. *)

    val t_of_sexp : Sexplib.Sexp.t -> t
    (** [t_of_sexp sexp] converts a S-expression [sexp] into a position [t]. *)

    val sexp_of_t : t -> Sexplib.Sexp.t
    (** [sexp_of_t pos] converts a position [t] into a S-expression. *)

    val to_int : t -> int
    (** [to_int pos] returns the integer representation of the position [pos].
    *)
  end

  val to_string : t -> string
  (** [to_string token] returns the string representation of the token [token].
  *)

  val compare : String.t -> String.t -> int
  (** [compare token1 token2] compares two tokens [token1] and [token2]. *)
end

module TokenPair : sig
  (** The [TokenPair] module defines the type and operations for pairs of tokens
      with their positions. *)

  type t = (Token.t * Token.Pos.t) * (Token.t * Token.Pos.t)
  (** The token pair type is represented as a pair of (token, position) tuples.
  *)

  val create :
    Token.t ->
    Token.Pos.t ->
    Token.t ->
    Token.Pos.t ->
    (Token.t * Token.Pos.t) * (Token.t * Token.Pos.t)
  (** [create ft fp tt tp] creates a token pair from the first token [ft] at
      position [fp] and the second token [tt] at position [tp]. *)

  val st : t -> Token.t * Token.Pos.t
  (** [st token_pair] returns the starting token and its position from the
      [token_pair]. *)

  val en : t -> Token.t * Token.Pos.t
  (** [en token_pair] returns the ending token and its position from the
      [token_pair]. *)

  val distance_vec : t -> int
  (** [distance_vec token_pair] returns the distance vector between the two
      tokens in the [token_pair]. *)

  val distance : t -> int
  (** [distance token_pair] returns the absolute distance between the two tokens
      in the [token_pair]. *)
end

module TokenEntry : sig
  (** The [TokenEntry] module defines the type and operations for token entries
      in documents. *)

  module Flags : sig
    (** The [Flags] module defines the type and operations for token entry
        flags. *)

    type t = { title : bool; directory : bool; extension : bool; source : bool }
    (** The flags type represents various attributes of a token entry.
        - [title]: indicates if the token is in the title.
        - [directory]: indicates if the token is in the directory.
        - [extension]: indicates if the token is in the file extension.
        - [source]: indicates if the token is in the source code. *)

    val t_of_sexp : Sexplib.Sexp.t -> t
    (** [t_of_sexp sexp] converts a S-expression [sexp] into a flags [t]. *)

    val sexp_of_t : t -> Sexplib.Sexp.t
    (** [sexp_of_t flags] converts a flags [t] into a S-expression. *)

    val empty : t
    (** [empty] creates an empty flags instance with all attributes set to
        false. *)

    val create : bool -> bool -> bool -> bool -> t
    (** [create t d e s] creates a flags instance with the specified attributes:
        - [t]: title flag - true if the token is in the title
        - [d]: directory flag - true if the token is in the directory
        - [e]: extension flag - true if the token is in the file extension
        - [s]: source flag - true if the token is in the source code *)

    val from_string : string -> t
    (** [from_string s] creates a flags instance from a string [s] where each
        character represents a flag:
        - [T] for title
        - [D] for directory
        - [E] for extension
        - [S] for source *)

    val to_string : t -> string
    (** [to_string flags] converts a flags instance [flags] into a string
        representation. See [from_string] *)

    val create_title : t
    (** [create_title] creates a flags instance with only the title flag set to
        true. *)

    val create_directory : t
    (** [create_directory] creates a flags instance with only the directory flag
        set to true. *)

    val create_extension : t
    (** [create_extension] creates a flags instance with only the extension flag
        set to true. *)

    val create_source : t
    (** [create_source] creates a flags instance with only the source flag set
        to true. *)

    val set_title : t -> t
    (** [set_title flags] sets the title flag of the [flags] instance to true.
    *)

    val set_directory : t -> t
    (** [set_directory flags] sets the directory flag of the [flags] instance to
        true. *)

    val set_extension : t -> t
    (** [set_extension flags] sets the extension flag of the [flags] instance to
        true. *)

    val set_source : t -> t
    (** [set_source flags] sets the source flag of the [flags] instance to true.
    *)

    val title : t -> bool
    (** [title flags] returns true if the title flag of the [flags] instance is
        set to true. *)

    val directory : t -> bool
    (** [directory flags] returns true if the directory flag of the [flags]
        instance is set to true. *)

    val extension : t -> bool
    (** [extension flags] returns true if the extension flag of the [flags]
        instance is set to true. *)

    val source : t -> bool
    (** [source flags] returns true if the source flag of the [flags] instance
        is set to true. *)
  end

  type t = { token : Token.t; positions : Token.Pos.t list; flags : Flags.t }
  (** The token entry type represents a token in a document along with its
      positions and flags.
      - [token]: the token string
      - [positions]: a list of positions where the token appears in the document
      - [flags]: attributes of the token entry *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  (** [t_of_sexp sexp] converts a S-expression [sexp] into a token entry [t]. *)

  val sexp_of_t : t -> Sexplib.Sexp.t
  (** [sexp_of_t token_entry] converts a token entry [t] into a S-expression. *)

  val create : Token.t -> Token.Pos.t list -> Flags.t -> t
  (** [create token positions flags] creates a token entry with the specified
      [token], list of [positions], and [flags]. *)

  val create_title : Token.t -> Token.Pos.t list -> t
  (** [create_title token positions] creates a token entry with the specified
      [token] and list of [positions], setting the title flag to true. *)

  val create_directory : Token.t -> Token.Pos.t list -> t
  (** [create_directory token positions] creates a token entry with the
      specified [token] and list of [positions], setting the directory flag to
      true. *)

  val create_extension : Token.t -> Token.Pos.t list -> t
  (** [create_extension token positions] creates a token entry with the
      specified [token] and list of [positions], setting the extension flag to
      true. *)

  val create_source : Token.t -> Token.Pos.t list -> t
  (** [create_source token positions] creates a token entry with the specified
      [token] and list of [positions], setting the source flag to true. *)

  val token : t -> Token.t
  (** [token token_entry] returns the token string from the [token_entry]. *)

  val token_length : t -> int
  (** [token_length token_entry] returns the length of the token in the
      [token_entry]. *)

  val positions : t -> Token.Pos.t list
  (** [positions token_entry] returns the list of positions from the
      [token_entry]. *)

  val count : t -> int
  (** [count token_entry] returns the number of positions in the [token_entry].
  *)

  val has_positions : t -> bool
  (** [has_positions token_entry] returns true if the [token_entry] has at least
      one position. *)

  val add_position : Token.Pos.t -> t -> t
  (** [add_position pos token_entry] adds a new position [pos] to the
      [token_entry], maintaining the positions in sorted order. *)

  val flags : t -> Flags.t
  (** [flags token_entry] returns the flags from the [token_entry]. *)

  val set_title : t -> t
  (** [set_title token_entry] sets the title flag of the [token_entry] to true.
  *)

  val set_directory : t -> t
  (** [set_directory token_entry] sets the directory flag of the [token_entry]
      to true. *)

  val set_extension : t -> t
  (** [set_extension token_entry] sets the extension flag of the [token_entry]
      to true. *)

  val set_source : t -> t
  (** [set_source token_entry] sets the source flag of the [token_entry] to
      true. *)

  val in_range : Token.Pos.t -> int -> t -> (Token.Pos.t * int) list
  (** [in_range from range token_entry] returns a list of (position, distance)
      pairs for positions in the [token_entry] that are within the specified
      [range] from the [from] position. *)

  val closest_distance :
    t -> t -> ((Token.t * Token.Pos.t) * (Token.t * Token.Pos.t)) option
  (** [closest_distance entry_a entry_b] returns the closest token pair between
      [entry_a] and [entry_b], or [None] if either entry has no positions. *)
end

module Parser : sig
  (** The [Parser] module provides functions for tokenizing text and parsing
      documents into token entries. *)

  val parse :
    char list -> ?min_token_length:int -> Document.t -> TokenEntry.t list
  (** [parse char_list ?min_token_length document] parses the given [document]
      into a list of [TokenEntry.t] using the specified [char_list] as
      delimiters. The optional [min_token_length] parameter can be used to
      filter out tokens shorter than the specified length. *)
end
