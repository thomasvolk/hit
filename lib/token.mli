type t = {
  word: string;
  positions: int list;
}

val create : string -> int list -> t

module Parser : sig
  val parse : string -> t list
end
