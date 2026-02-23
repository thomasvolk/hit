(** Module for managing token and document tables in a text processing system.
*)

module TokenMap : Map.S with type key = Text.Token.t

module DocumentTable : sig
  (** A module representing a document table that maps documents to token
      entries and their positions. *)

  module DocumentTableId : sig
    val prefix : string
  end

  module Id : Reference.IdType

  type t = {
    id : Id.t;
    map :
      (Text.TokenEntry.Flags.t * Text.Token.Pos.t list) Document.DocumentMap.t;
  }
  (** The document table data type *)

  val id : t -> Id.t
  (** [id document_table] returns the [id] of the document table *)

  val empty : Id.t -> t
  (** [empty id] creates an empty document table with the given [id] *)

  val add :
    Document.Id.t -> Text.TokenEntry.Flags.t * Text.Token.Pos.t list -> t -> t
  (** [add document_id (flags, pos_list) document_table] adds a new entry to the
      [document_table] for the given [document_id] with the specified [flags]
      and list of positions [pos_list] *)

  val get : Document.Id.t -> 'a Document.DocumentMap.t -> 'a option
  (** [get document_id document_map] retrieves the entry for the given
      [document_id] from the [document_map], returning [None] if it does not
      exist *)

  val all :
    t ->
    (Document.Id.t * (Text.TokenEntry.Flags.t * Text.Token.Pos.t list)) list
  (** [all document_table] returns a list of all entries in the [document_table]
      as (document_id, (flags, pos_list)) pairs *)

  val filter :
    (Document.Id.t -> Text.TokenEntry.Flags.t * Text.Token.Pos.t list -> bool) ->
    t ->
    t
  (** [filter predicate document_table] filters the entries in the
      [document_table] based on the given [predicate] function *)

  val size : t -> int
  (** [size document_table] returns the number of entries in the
      [document_table] *)

  val merge : t -> t -> t
  (** [merge document_table_a document_table_b] merges two document tables,
      preferring entries from [document_table_a] in case of conflicts *)
end

module DocumentTableMap : Map.S with type key = DocumentTable.Id.t

module TokenTable : sig
  (** A module representing a token table that maps tokens to document table
      IDs. *)

  type t = DocumentTable.Id.t TokenMap.t
  (** The token table data type *)

  val add : TokenMap.key -> 'a -> 'a TokenMap.t -> 'a TokenMap.t
  (** [add token dt_id token_table] adds a mapping from the given [token] to the
      specified [dt_id] in the [token_table] *)

  val get : TokenMap.key -> 'a TokenMap.t -> 'a option
  (** [get token token_table] retrieves the document table ID associated with
      the given [token] from the [token_table], returning [None] if it does not
      exist *)

  val remove : TokenMap.key -> 'a TokenMap.t -> 'a TokenMap.t
  (** [remove token token_table] removes a token *)

  val find_all :
    (TokenMap.key -> bool) -> 'a TokenMap.t -> (TokenMap.key * 'a) list
  (** [find_all predicate token_table] returns a list of all (token,
      document_table_id) pairs in the [token_table] that satisfy the given
      [predicate] function *)

  val to_list : 'a TokenMap.t -> (TokenMap.key * 'a) list
  (** [to_list token_table] converts the [token_table] to a list of (token,
      document_table_id) pairs *)

  val empty : 'a TokenMap.t
  (** [empty] creates an empty token table *)

  val size : 'a TokenMap.t -> int
  (** [size token_table] returns the number of entries in the [token_table] *)

  val merge : 'a TokenMap.t -> 'a TokenMap.t -> 'a TokenMap.t
  (** [merge token_table_a token_table_b] merges two token tables, preferring
      entries from [token_table_a] in case of conflicts *)
end

module DocumentIdSet : Set.S with type elt = Document.Id.t

module DocumentRegister : sig
  type t = DocumentIdSet.t
  val empty : t
  val add : Document.Id.t -> t -> t
  val remove : Document.Id.t -> t -> t
  val contains : Document.Id.t -> t -> bool
  val size : t -> int
  val to_list : t -> Document.Id.t list
end
