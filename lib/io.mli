(** Module for handling storage operations for documents, token tables, and
    document tables. *)

open Table

val read_file : string -> string
(** [read_file filename] reads the content of the file with the given [filename]
    and returns it as a string. *)

val hash_to_path : string -> string
(** [hash_to_path hash] converts a hash string into a file path by splitting it
    into folders. *)

val path_to_hash : string -> string
(** [path_to_hash path] converts a file path back into a hash string by removing
    the folder structure. *)

val find_all_files : predicate:(string -> bool) -> string -> string list
(** [find_all_files ~predicate path] recursively finds all files in the
    directory at [path] that satisfy the given [predicate]. *)

module DocumentIdSet : Set.S with type elt = Document.Id.t
(** A set module for Document IDs. *)

module type StorageType = sig
  type t
  type config

  val create : config -> t
  (** [create config] creates a new storage instance with the given [config]. *)

  val load_index_config : t -> Config.IndexConfig.t
  (** [load_index_storage] loads the index configuration from the [storage]. *)

  val save_index_config : Config.IndexConfig.t -> t -> unit
  (** [save_index_config index_config storage] saves the given [index_config] to
      the [storage]. *)

  val index_config_exists : t -> bool
  (** [index_config_exists storage] checks if the index configuration exists in
      the [storage]. *)

  val load_doc_table : DocumentTable.Id.t -> t -> DocumentTable.t
  (** [load_doc_table doc_table_id storage] loads the document table with the
      given [doc_table_id] from the [storage]. *)

  val save_doc_table : DocumentTable.t -> t -> unit
  (** [save_doc_table doc_table storage] saves the given [doc_table] to the
      [storage]. *)

  val load_token_table : t -> TokenTable.t
 (** [load_token_table storage] loads the token table from the [storage]. *)

  val save_token_table : TokenTable.t -> t -> unit
  (** [save_token_table token_table storage] saves the given [token_table] to
      the [storage]. *)

  val load_doc : Document.Id.t -> t -> Document.t
  (** [load_doc document_id storage] loads the document with the given
      [document_id] from the [storage]. *)

  val load_doc_opt : Document.Id.t -> t -> Document.t option
  (** [load_doc_opt document_id storage] loads the document with the given
      [document_id] from the [storage], returning [None] if it does not exist.
  *)

  val get_all_doc_ids : t -> DocumentIdSet.t
  (** [get_all_doc_ids storage] retrieves all document IDs stored in the
      [storage]. *)

  val save_doc : Document.t -> t -> unit
  (** [save_doc document storage] saves the given [document] to the [storage].
  *)

  val delete_doc : Document.Id.t -> t -> bool
  (** [delete_doc document_id storage] deletes the document with the given
      [document_id] from the [storage], returning [true] if the document was
      successfully deleted, or [false] if it did not exist. *)

  val doc_exists : Document.Id.t -> t -> bool
  (** [doc_exists document_id storage] checks if a document with the given
      [document_id] exists in the [storage]. *)

  val write_doc_register : DocumentRegister.t -> t -> unit
  (** [write_doc_register doc_register storage] writes the given [doc_register] to
      the [storage]. *)

  val read_doc_register : t -> DocumentRegister.t
  (** [read_doc_register storage] reads the document register from the [storage]. *)

  val lock : ?force:bool -> t -> unit
  (** [lock ?force storage] acquires a write lock on the [storage]. If [force]
      is true, it will override any existing lock. *)

  val unlock : t -> unit
  (** [unlock storage] releases the write lock on the [storage]. *)

  val with_lock : ?force:bool -> (unit -> 'a) -> t -> 'a
  (** [with_lock ?force f storage] executes the function [f] while holding a
      write lock on the [storage]. If [force] is true, it will override any
      existing lock. *)
end

module type StorageInstance = sig
  (** This module type represents an instance of a storage implementation. *)

  module Impl : StorageType

  val t : Impl.t
end

val file_storage : string -> (module StorageInstance)
(** [file_storage path] creates a file-based storage instance at the given
    [path]. *)

val in_memory_storage : int * int -> (module StorageInstance)
(** [in_memory_storage (min_doc_table_hashtbl_size, min_document_hashtbl_size)]
    creates a in memory-based storage instance. *)

val storage :
  (module StorageType with type config = 'a) -> 'a -> (module StorageInstance)
(** [storage (module S) config] creates a storage instance using the storage
    type [S] and the provided [config]. *)
