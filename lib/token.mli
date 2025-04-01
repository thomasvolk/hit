type t = {
  word: string;
  positions: int list;
}

val of_string_list : string -> t list

val create : string -> int list -> t
