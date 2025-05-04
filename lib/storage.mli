
module type StorageType = sig
  type t
  type config
  type k
  type v

  val create : config -> t
  
  val load : k -> t -> v

  val save : v -> t -> unit
end


module Make : functor (P: StorageType) -> sig
  type t = P.t
  type config = P.config
  type k = P.k
  type v = P.v

  val create : config -> t

  val load : k -> t -> v

  val save : v -> t -> unit
end


module type StorageInstance = sig
  module StorageType : StorageType
  val t : StorageType.t
end


val storage_instance :
  (module StorageType with type config = 'a) ->
  'a -> (module StorageInstance)


module IndexEntryFile : sig
  type t = { 
    base_path : string;
  }
  type config = string
  type v = Index.Entry.t
  type k = Index.Term.t

  val create : config -> t

  val load : k -> t -> v

  val save : v -> t -> unit
end


val read_file : string -> string

val write_file : string -> string -> unit


module Path : sig

  type t = private string

  val of_ref : Ref.t -> t

  val to_string : t -> string

end

