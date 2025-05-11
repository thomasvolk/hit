
module type Doc_table_storage_type = sig
  type t
  type config

  val create : config -> t
  
  val load : Ref.t -> t -> Doc_table.t

  val save : Ref.t -> Doc_table.t -> t -> unit
end


module type Doc_table_storage = sig
  module Impl : Doc_table_storage_type
  val t : Impl.t
end


val doc_table_storage :
  (module Doc_table_storage_type with type config = 'a) ->
  'a -> (module Doc_table_storage)


module Doc_table_file : sig
  type t = { 
    base_path : string;
  }
  type config = string

  val create : config -> t
  
  val load : Ref.t -> t -> Doc_table.t

  val save : Ref.t -> Doc_table.t -> t -> unit
end


val read_file : string -> string

val write_file : string -> string -> unit


module Path : sig

  type t = private string

  val of_ref : Ref.t -> t

  val to_string : t -> string

end

