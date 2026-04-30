module Query : sig
  type t =
    | Eq of string
    | Or of t list
    | And of t list
  [@@deriving sexp]

  val from_string : string -> t
end

type t = { path : string }
type doc_files = { tokens_file : string; doc_file : string }

val create : string -> t
val add :
  t ->
  ?tokenizer:(?token_start_char:int -> ?separators:char list -> ?min_token_length:int -> string -> string list) ->
  string ->
  string ->
  Doc.Id.t
val delete : t -> Doc.Id.t -> unit
val query : t -> string -> Doc.Id.t list
val get_doc : t -> Doc.Id.t -> Doc.t
val doc_files : t -> Doc.Id.t -> doc_files
val dump : t -> Core.Sexp.t
