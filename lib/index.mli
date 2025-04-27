
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

module FileIo : sig
  type config = { 
    base_path : string;
  }

  val load : string -> config -> t
  (** [open_register word] returns the register for the given [word] *)

  val save : string -> t -> config -> unit
  (** [store_register word register] stores the register *)
end
