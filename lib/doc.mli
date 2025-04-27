
type t = private {
  path: string;
  origin: string
}

val source : t -> string

val name : t -> string

val create : string -> string -> t
