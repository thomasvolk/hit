open Sexplib.Std

module Checksum = struct
  type t = string [@@deriving sexp]

  let create cnt = Digest.MD5.string cnt |> Digest.MD5.to_hex
end

type t = { source : string; path : string; checksum : Checksum.t }
  [@@deriving sexp]

let create s p c = { source = s; path = p; checksum = c }
let path m = m.path
let name m = Filename.basename m.path
let directory m = Filename.dirname m.path
let extension m = Filename.extension m.path

let title m =
  let n = name m in
  match Filename.extension n with "" -> n | _ -> Filename.chop_extension n
  let checksum m = m.checksum

