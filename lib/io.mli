
module type Operator = sig
  type t
  type config
  
  val load : string -> config -> t

  val save : string -> t -> config -> unit
end

module Make : functor (O: Operator) -> sig
  type t = O.t
  type config = O.config

  val load : string -> config -> t

  val save : string -> t -> config -> unit
end

val read_file : string -> string

val write_file : string -> string -> unit
