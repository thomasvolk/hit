
module type Persistence = sig
  type t
  type config
  
  val load : string -> config -> t

  val save : t -> config -> unit
end


module Make : functor (P: Persistence) -> sig
  type t = P.t
  type config = P.config

  val load : string -> config -> t

  val save : t -> config -> unit
end


val read_file : string -> string

val write_file : string -> string -> unit


module Path : sig

  type t = private string

  val of_ref : Ref.t -> t

  val to_string : t -> string

end


module TermIndexFile : sig
  type t = Index.TermIndex.t

  type config = { 
    base_path : string;
  }

  val create : string -> config

  val load : string -> config -> t

  val save : t -> config -> unit
end
