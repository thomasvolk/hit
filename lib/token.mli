type t = {
  word: string;
  positions: int list;
} [@@deriving sexp]

val create : string -> int list -> t

module Parser : sig
  val parse : string -> t list
end
