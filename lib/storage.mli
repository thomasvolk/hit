
module type StorageType = sig
  type t
  type config
  type v

  val create : config -> t
  
  val load : Ref.t -> t -> v

  val save : Ref.t -> v -> t -> unit
end


module Make : functor (P: StorageType) -> sig
  type t = P.t
  type config = P.config
  type v = P.v

  val create : config -> t
  
  val load : Ref.t -> t -> v

  val save : Ref.t -> v -> t -> unit
end


module IndexEntryFile : sig
  type t = { 
    base_path : string;
  }
  type config = string
  type v = Index.Entry.t

  val create : config -> t
  
  val load : Ref.t -> t -> v

  val save : Ref.t -> v -> t -> unit
end


val read_file : string -> string

val write_file : string -> string -> unit


module Path : sig

  type t = private string

  val of_ref : Ref.t -> t

  val to_string : t -> string

end

