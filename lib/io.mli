
module type Persistence = sig
  type t
  type config
  
  val load : string -> config -> t

  val save : string -> t -> config -> unit
end

module Make : functor (P: Persistence) -> sig
  type t = P.t
  type config = P.config

  val load : string -> config -> t

  val save : string -> t -> config -> unit
end

val read_file : string -> string

val write_file : string -> string -> unit

module TermFile : sig
  type t = Term.t

  type config = { 
    base_path : string;
  }

  val create : string -> config

  val load : string -> config -> t

  val save : string -> t -> config -> unit
end
