
type t = private string

val create : string -> t

val to_string : t -> string

val of_string : string -> t

val compare : t -> t -> int
