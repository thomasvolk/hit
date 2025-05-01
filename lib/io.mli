
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

module TermFile : sig
  type t = Term.t

  type config = { 
    base_path : string;
  }

  val create : string -> config

  val load : string -> config -> t
  (** [open_register word] returns the register for the given [word] *)

  val save : string -> t -> config -> unit
  (** [store_register word register] stores the register *)
end
