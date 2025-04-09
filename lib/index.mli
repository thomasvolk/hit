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
  module EntryMap : Map.S with type key = string

  module Entry : sig
    type t = private string * int list
    exception InvalidRef of string

    val create : string -> int list -> t
  end

  type t = Entry.t EntryMap.t

  val empty : t

  val of_string : string -> t

  val to_string : t -> string

  val add : Entry.t -> t -> t

  val size : t -> int
end

module Hash : sig
  type t = string

  val create : string -> t

  val to_path : t -> string
end

val register_path : t -> string

val open_register : string -> t -> Register.t
(** [open_register word index] returns the register for the given [word] *)

val store_register : string -> Register.t -> t -> unit
(** [store_register word register index] stores the register *)
