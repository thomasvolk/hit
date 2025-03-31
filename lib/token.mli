type t = {
  word: string;
  positions: int list;
}

val of_string : string -> t list

val create : string -> int list -> t
