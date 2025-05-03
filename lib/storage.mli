
module type StorageType = sig
  type t
  type k
  type config
  
  val load : string -> config -> t

  val save : t -> config -> unit
end


module Make : functor (P: StorageType) -> sig
  type t = P.t
  type k = P.k
  type config = P.config

  val load : string -> config -> t

  val save : t -> config -> unit
end


module TermIndexFile : sig
  type t = Index.TermIndex.t
  type k = Index.Term.t

  type config = { 
    base_path : string;
  }

  val create : string -> config

  val load : k -> config -> t

  val save : t -> config -> unit
end


val read_file : string -> string

val write_file : string -> string -> unit


module Path : sig

  type t = private string

  val of_ref : Ref.t -> t

  val to_string : t -> string

end

