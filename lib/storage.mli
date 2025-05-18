
module type Storage_type = sig
  type t
  type config
  type e

  val create : config -> t
  
  val load : Ref.t -> t -> e

  val save : Ref.t -> e -> t -> unit
end


module type Doc_table_storage_instance_type = Storage_type with type e = Doc_table.t


module type Doc_table_storage_instance = sig
  module Impl : Doc_table_storage_instance_type
  val t : Impl.t
end


val doc_table_storage :
  (module Doc_table_storage_instance_type with type config = 'a) ->
  'a -> (module Doc_table_storage_instance)


module Doc_table_file : sig
  type t = { 
    base_path : string;
  }
  type config = string
  type e = Doc_table.t

  val create : config -> t
  
  val load : Ref.t -> t -> e

  val save : Ref.t -> e -> t -> unit
end


val read_file : string -> string

val write_file : string -> string -> unit


module Path : sig

  type t = private string

  val of_ref : Ref.t -> t

  val to_string : t -> string

end

