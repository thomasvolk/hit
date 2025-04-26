
module Document : sig
  type t = private {
    path: string;
    origin: string
  }
  val source : t -> string

  val name : t -> string

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

  module FileIo : sig
    type r = t
    val load : string -> r
    (** [open_register word] returns the register for the given [word] *)

    val save : string -> r -> unit
    (** [store_register word register] stores the register *)
  end
end

module Hash : sig
  type t = string

  val create : string -> t

  val to_path : t -> string
end
