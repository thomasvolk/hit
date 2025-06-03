open Sexplib.Std

(* A Token is a word that can be searched *)

type t = string [@@deriving sexp]

module Pos = struct
  type t = int [@@deriving sexp]

  let to_int t = t
end

let to_string t = t

let compare a b = String.compare a b


