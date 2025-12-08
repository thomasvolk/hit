(** Module for generating document previews based on search query results. *)

module Preview : sig
  (** The [Preview] module provides functionality to create previews of
      documents based on search query results. It defines a type for preview
      parts, which can be either plain text or highlighted tokens, and functions
      to generate previews from tokens and query results. *)

  type part =
    | Text of string
    | Token of Text.Token.t
        (** The [part] type represents a part of the preview, which can be
            either plain text ([Text]) or a highlighted token ([Token]). *)

  type t = part list
  (** The [t] type represents a preview as a list of [part]s. *)

  val of_tokens : string -> (Text.Token.t * int) list -> part list
  (** [of_tokens doc_text token_positions] creates a preview from the given
      [doc_text] and a list of [token_positions], where each position is a pair
      of a [Text.Token.t] and its starting index in the document text. *)

  val create : Document.t -> Index.QueryResult.t -> part list
  (** [create document query_result] generates a preview for the given
      [document] based on the [query_result], highlighting the relevant tokens.
  *)

  val shorten_txt : ?max_len:int -> string -> string
  (** [shorten_txt ?max_len text] shortens the given [text] to a maximum length
      of [max_len] characters, adding ellipses if necessary. The default value
      for [max_len] is 200. *)

  val shorten : ?max_len:int -> part list -> part list
  (** [shorten ?max_len parts] shortens the given list of [parts] to a maximum
      length of [max_len] characters, preserving the structure of the preview.
      The default value for [max_len] is 200. *)
end
