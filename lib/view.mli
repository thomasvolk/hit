module Preview : sig
  type part = Text of string | Token of Text.Token.t
  type t = part list

  val of_tokens : string -> (Text.Token.t * int) list -> part list
  val create : Document.t -> Index.QueryResult.t -> part list
  val shorten_txt : ?max_len:int -> string -> string
  val shorten : ?max_len:int -> part list -> part list
end
