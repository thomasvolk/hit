type t = {
  token: string;
  pos: int;
}

val parse : string -> t list

val create : string -> int -> t
