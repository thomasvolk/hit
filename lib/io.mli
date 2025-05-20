
val read_file : string -> string

val write_file : string -> string -> unit


module type StorageType = sig
  type t
  type config

  val create : config -> t
  
  val load_doc_table : Ref.t -> t -> Doc_table.t

  val save_doc_table : Ref.t -> Doc_table.t -> t -> unit
end


module type StorageInstance = sig
  module Impl : StorageType
  val t : Impl.t
end


val doc_table_storage :
  (module StorageType with type config = 'a) ->
  'a -> (module StorageInstance)


val doc_table_file_storage : string -> (module StorageInstance)
