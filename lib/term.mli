
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
