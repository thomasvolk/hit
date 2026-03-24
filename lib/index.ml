open Sexplib.Std

module Query = struct
  type t =
    | Eq of string
    | Sw of string
    | Ew of string
    | Or of t list [@sexp.list]
    | And of t list [@sexp.list]
  [@@deriving sexp]

  let from_string s = t_of_sexp (Sexplib.Sexp.of_string s)

  module type QueryType = sig
    val from_string : string -> t
  end
end

type t = { path : string }

let create path = { path }

let add t path content =
  let _tokens =
    Token.from_string path @ Token.from_string content |> Token.with_orders
  in
  let doc = Doc.create path (Doc.Checksum.create content) in
  let doc_id = Doc.Id.create path in
  let doc_dir = Doc.Id.to_path doc_id in
  let open Io in
  let trx =
    Trx.empty
    |> Trx.add_write_file (t.path // doc_dir // "doc.hit") (Doc.sexp_of_t doc)
  in
  execute_transaction (t.path // "trx" // (Doc.Id.to_string doc_id ^ ".hit")) trx
