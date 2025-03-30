type t = {
  token: string;
  positions: int list;
}

val parse : string -> t list

val create : string -> int list -> t
