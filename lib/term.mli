
module EntryMap : Map.S with type key = Ref.t

module Entry : sig
  type t = private Ref.t * int list
  exception InvalidEntry of string

  val create : Ref.t -> int list -> t
end

type t = Entry.t EntryMap.t

val empty : t

val of_string : string -> t

val to_string : t -> string

val add : Entry.t -> t -> t

val size : t -> int
