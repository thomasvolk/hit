type t = {
  token: string;
  row: int;
  col: int;
}

val parse : string -> t list

val create : string -> int -> int -> t
