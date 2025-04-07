type t = {
  path: string;
}

module Document : sig
  type t = private {
    path: string;
    origin: string
  }
  val source : t -> string

  val name : t -> string

  val ref : t -> string

  val create : string -> string -> t
end

module Register : sig
  module RefMap : Map.S with type key = string

  module Ref : sig
    type t = private string * int list
    exception InvalidRef of string

    val create : string -> int list -> t
  end

  type t = {
    word: string;
    entries: Ref.t RefMap.t
  }

  val empty : string -> t

  val of_string : string -> t

  val to_string : t -> string

  val add : Ref.t -> t -> t

  val size : t -> int
end

val register_path : t -> string

val open_register : string -> t -> Register.t
(** [open_register index word] returns the register for the given [word] *)

val store_register : Register.t -> t -> unit
(** [store_register index register] stores the register *)
