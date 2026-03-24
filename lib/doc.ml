open Sexplib.Std

module Checksum : sig
  type t
  val create : string -> t
  val t_of_sexp : Core.Sexp.t -> t
  val sexp_of_t : t -> Core.Sexp.t
end = struct
  type t = string [@@deriving sexp]
  let create cnt = Digest.MD5.string cnt |> Digest.MD5.to_hex
end

type t = { path : string; checksum : Checksum.t } [@@deriving sexp]

let create path checksum = { path; checksum }
let path d = d.path
let name d = Filename.basename d.path
let directory d = Filename.dirname d.path
let extension d = Filename.extension d.path
let checksum d = d.checksum

let title d =
  let n = name d in
  match Filename.extension n with "" -> n | _ -> Filename.chop_extension n

module TokenRefs = struct
  type t = string list [@@deriving sexp]

  let empty = []
  let add s t = s :: t |> List.sort_uniq String.compare
end
