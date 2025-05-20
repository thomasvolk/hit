
val read_file : string -> string

val write_file : string -> string -> unit


module type StorageType = sig
  type t
  type config
  type e

  val create : config -> t
  
  val load : Ref.t -> t -> e

  val save : Ref.t -> e -> t -> unit
end


module type StorageInstance = sig
  type v
  module Impl : StorageType with type e = v
  val t : Impl.t
end


val doc_table_storage :
  (module StorageType with type config = 'a and type e = Doc_table.t) ->
  'a -> (module StorageInstance with type v = Doc_table.t)


val doc_table_file_storage : string -> (module StorageInstance with type v = Doc_table.t)
