
module type StorageType = sig
  type t = { 
    base_path : string;
  }
  type config

  val create : config -> t
  
  val load_doc_table : Ref.t -> t -> Doc_table.t

  val save_doc_table : Ref.t -> Doc_table.t -> t -> unit
end


module type StorageInstance = sig
  module StorageType : StorageType
  val t : StorageType.t
end


val storage_instance :
  (module StorageType with type config = 'a) ->
  'a -> (module StorageInstance)


module FileSystem : sig
  type t = { 
    base_path : string;
  }
  type config = string

  val create : config -> t
  
  val load_doc_table : Ref.t -> t -> Doc_table.t

  val save_doc_table : Ref.t -> Doc_table.t -> t -> unit
end


val read_file : string -> string

val write_file : string -> string -> unit


module Path : sig

  type t = private string

  val of_ref : Ref.t -> t

  val to_string : t -> string

end

