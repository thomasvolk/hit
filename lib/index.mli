type t = {
  path: string;
}

module Document : sig
  type t = private {
    path: string;
    origin: string;
  }
  val source : t -> string

  val name : t -> string

  val ref : t -> string

  val create : string -> string -> t
end

module Entry : sig
  module RefMap : Map.S with type key = string

  module Ref : sig
    type t = string * int list
  end

  type t = Ref.t RefMap.t

  val empty : t

  val of_string : string -> t

  val to_string : t -> string

  val add : Ref.t -> t -> t

  val size : t -> int
end

val entry_path : t -> string

val entry : t -> string -> Entry.t

