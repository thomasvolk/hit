
module type Operator = sig
  type t
  val load : string -> t

  val save : string -> t -> unit
end

module Make : functor (O: Operator) -> sig
  type t = O.t
  val load : string -> t

  val save : string -> t -> unit
end

val read_file : string -> string

val write_file : string -> string -> unit
