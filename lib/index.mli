module DocumentMap = Document.DocumentMap
(** The index module *)

type t = {
  token_table : Table.TokenTable.t;
  doc_tables : Table.DocumentTable.t Table.DocumentTableMap.t;
  config : Config.IndexConfig.t;
}
(** The search index data type *)

module QueryResult : sig
  (** A query result contains the document id and the list of token entries that
      matched the query in that document. *)

  type t = { doc_id : Document.Id.t; token_entries : Text.TokenEntry.t list }
  (** The query result data type *)

  val create : Document.Id.t -> Text.TokenEntry.t list -> t
  (** [create document_id token_entry_list] creates a query result for one
      document *)

  val doc_id : t -> Document.Id.t
  (** [doc_id query_result] returns the [document_id] *)

  val token_entries : t -> Text.TokenEntry.t list
  (** [token_entries query_result] returns the [token_entries] *)

  val closest_distances : t -> Text.TokenPair.t list
  (** [closest_distance query_result] creates a list of [token_pair_list] with
      the closest distance to each other *)

  val score : Config.IndexConfig.t -> t -> int
  (** [score query_result] calculates the score *)

  val compare : Config.IndexConfig.t -> t -> t -> int
  (** [compare query_result_a query_result_b] compares two [query_results] *)
end

module type IndexReaderType = sig
  (** This module type defines the interface for reading from an index. *)

  val get_doc : Document.Id.t -> Document.t
  (** [get_doc document_id] retrieves the document with the given [document_id]
  *)

  val get_entries : t -> string -> (Document.Id.t * Text.TokenEntry.t list) list
  (** [get_entries index token] retrieves the list of documents and their
      corresponding token entries for the given [token] *)

  val find_entries :
    t -> (string -> bool) -> (Document.Id.t * Text.TokenEntry.t list) list
  (** [find_entries index predicate] retrieves the list of documents and their
      corresponding token entries for tokens that satisfy the given [predicate]
  *)
end

module Query : sig
  (** This module defines the query language for searching the index. *)

  type idx_t := t
  (** The index type alias *)

  type t =
    | Eq of string
    | Sw of string
    | Ew of string
    | Or of t list
    | And of t list
        (** The query data type
            - [Eq token]: matches documents containing the exact token
            - [Sw token]: matches documents containing tokens that start with
              the given prefix
            - [Ew token]: matches documents containing tokens that end with the
              given suffix
            - [Or query_list]: matches documents satisfying at least one query
              in the list
            - [And query_list]: matches documents satisfying all queries in the
              list *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  (** [t_of_sexp sexp] converts a S-expression [sexp] into a query [t] *)

  val sexp_of_t : t -> Sexplib.Sexp.t
  (** [sexp_of_t query] converts a query [t] into a S-expression *)

  val from_string : string -> t
  (** [from_string str] parses a string [str] into a query [t] *)

  module Make : (_ : IndexReaderType) -> sig
    (** This functor creates a module for executing queries on an index. *)

    val query : t -> idx_t -> QueryResult.t list
    (** [query query index] executes the [query] on the given [index] and
        returns a list of [QueryResult.t] *)

    val find_docs : string list -> idx_t -> QueryResult.t list
    (** [find_docs token_list index] finds documents containing all tokens in
        the [token_list] within the given [index] *)
  end
end

module Make : (_ : Io.StorageInstance) -> sig
  (** This functor creates a module for managing the search index. *)

  val load : unit -> t
  (** [load ()] loads the index from storage *)

  val init : unit -> unit
  (** [init ()] initializes the index storage *)

  val get_doc : Document.Id.t -> Document.t
  (** [get_doc document_id] retrieves the document with the given [document_id]
  *)

  val add_doc : Document.t -> t -> t
  (** [add_doc document index] adds the given [document] to the [index] *)

  val update_doc : Document.t -> t -> t
  (** [update_doc document index] updates the given [document] in the [index] *)

  val delete_doc : Document.Id.t -> 'a -> 'a
  (** [delete_doc document_id index] deletes the document with the given
      [document_id] from the [index] *)

  val get_entries : t -> string -> (Document.Id.t * Text.TokenEntry.t list) list
  (** [get_entries index token] retrieves the list of documents and their
      corresponding token entries for the given [token] *)

  val find_entries :
    t -> (string -> bool) -> (Document.Id.t * Text.TokenEntry.t list) list
  (** [find_entries index predicate] retrieves the list of documents and their
      corresponding token entries for tokens that satisfy the given [predicate]
  *)

  val flush : ?clear_cache:bool -> ?force:bool -> t -> t
  (** [flush ?clear_cache ?force index] flushes the [index] to storage. If
      [clear_cache] is true, it clears the in-memory cache after flushing. If
      [force] is true, a existing write lock will be ignored *)

  val garbage_collect : t -> t
  (** [garbage_collect index] remove references to not existing documents and
      remove documents which are not referenced [index] *)
end
