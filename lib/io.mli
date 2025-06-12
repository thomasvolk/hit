val read_file : string -> string
val write_file : string -> string -> unit

module type StorageType = sig
  type t
  type config

  val create : config -> t
  val load_doc_table : Model.DocumentTable.Id.t -> t -> Model.DocumentTable.t
  val save_doc_table : Model.DocumentTable.t -> t -> unit
  val load_token_table : t -> Model.TokenTable.t
  val save_token_table : Model.TokenTable.t -> t -> unit
  val load_doc : Model.Document.Id.t -> t -> Model.Document.t
  val save_doc : Model.Document.t -> t -> unit
end

module type StorageInstance = sig
  module Impl : StorageType

  val t : Impl.t
end

val storage :
  (module StorageType with type config = 'a) -> 'a -> (module StorageInstance)

val file_storage : string -> (module StorageInstance)
