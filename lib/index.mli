module DocumentMap = Document.DocumentMap

type t = {
  token_table : Table.TokenTable.t;
  doc_tables : Table.DocumentTable.t Table.DocumentTableMap.t;
  config : Config.IndexConfig.t;
}

module SearchResult : sig
  type t = { doc_id : DocumentMap.key; token_entries : Text.TokenEntry.t list }

  val create : DocumentMap.key -> Text.TokenEntry.t list -> t
  val from_tuple : DocumentMap.key * Text.TokenEntry.t list -> t
  val doc_id : t -> DocumentMap.key
  val token_entries : t -> Text.TokenEntry.t list
  val closest_distances : t -> ((string * int) * (string * int)) list
  val score : Config.IndexConfig.t -> t -> int
  val compare : Config.IndexConfig.t -> t -> t -> int
end

module type IndexType = sig
  val get_doc : DocumentMap.key -> Document.t

  val get_entries_for_token :
    t -> string -> (DocumentMap.key * Text.TokenEntry.t list) list

  val find_entries :
    t -> (string -> bool) -> (DocumentMap.key * Text.TokenEntry.t list) list
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

  module Make : (_ : IndexType) -> sig
    val query : t -> idx_t -> SearchResult.t list
    val find_docs : string list -> idx_t -> SearchResult.t list
  end
end

module Make : (_ : Io.StorageInstance) -> sig
  val load : unit -> t
  val init : unit -> unit
  val get_doc : DocumentMap.key -> Document.t
  val add_doc : Document.t -> t -> t
  val update_doc : Document.t -> t -> t
  val delete_doc : DocumentMap.key -> 'a -> 'a

  val get_entries_for_token :
    t -> string -> (DocumentMap.key * Text.TokenEntry.t list) list

  val find_entries :
    t -> (string -> bool) -> (DocumentMap.key * Text.TokenEntry.t list) list

  val flush : ?clear_cache:bool -> ?force:bool -> t -> t
  val garbage_collect : t -> t
end
