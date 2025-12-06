module DocumentMap = Document.DocumentMap
(** The index module *)

type t = {
  token_table : Table.TokenTable.t;
  doc_tables : Table.DocumentTable.t Table.DocumentTableMap.t;
  config : Config.IndexConfig.t;
}
(** The search index data type *)

module QueryResult : sig
  (** The query result data type *)
  type t = { doc_id : Document.Id.t; token_entries : Text.TokenEntry.t list }

  val create : Document.Id.t -> Text.TokenEntry.t list -> t
  (** [create document_id token_entry_list] creates a query result for one document *)

  val doc_id : t -> Document.Id.t
  (** [doc_id query_result] returns the [document_id] *)

  val token_entries : t -> Text.TokenEntry.t list
  (** [token_entries query_result] returns the [token_entries] *)

  val closest_distances : t -> Text.TokenPair.t list
  (** [closest_distance query_result] creates a list of [token_pair_list] with the closest distance to each other *)

  val score : Config.IndexConfig.t -> t -> int
  (** [score query_result] calculates the score *)

  val compare : Config.IndexConfig.t -> t -> t -> int
  (** [compare query_result_a query_result_b] compares two [query_results] *)
end

module type IndexReaderType = sig
  val get_doc : Document.Id.t -> Document.t
  val get_entries : t -> string -> (Document.Id.t * Text.TokenEntry.t list) list

  val find_entries :
    t -> (string -> bool) -> (Document.Id.t * Text.TokenEntry.t list) list
end

module Query : sig
  type idx_t := t

  type t =
    | Eq of string
    | Sw of string
    | Ew of string
    | Or of t list
    | And of t list

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val from_string : string -> t

  module Make : (_ : IndexReaderType) -> sig
    val query : t -> idx_t -> QueryResult.t list
    val find_docs : string list -> idx_t -> QueryResult.t list
  end
end

module Make : (_ : Io.StorageInstance) -> sig
  val load : unit -> t
  val init : unit -> unit
  val get_doc : Document.Id.t -> Document.t
  val add_doc : Document.t -> t -> t
  val update_doc : Document.t -> t -> t
  val delete_doc : Document.Id.t -> 'a -> 'a
  val get_entries : t -> string -> (Document.Id.t * Text.TokenEntry.t list) list

  val find_entries :
    t -> (string -> bool) -> (Document.Id.t * Text.TokenEntry.t list) list

  val flush : ?clear_cache:bool -> ?force:bool -> t -> t
  val garbage_collect : t -> t
end
