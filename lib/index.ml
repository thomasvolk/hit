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

module Index = struct
  type t = { path: string }
  let create path = { path = path }
  let add _t path content = 
    let _tokens = Token.from_string path @ Token.from_string content |> Token.with_orders in
    let _doc = Doc.create path (Doc.Checksum.create content) in
    ()
end
