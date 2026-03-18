open Sexplib.Std

module Checksum = struct
  type t = string [@@deriving sexp]

  let create cnt = Digest.MD5.string cnt |> Digest.MD5.to_hex
end

type t = { id: string; path : string; checksum : Checksum.t }
  [@@deriving sexp]

let create id path checksum = { id=id; path = path; checksum = checksum }
let id d = d.id
let path d = d.path
let name d = Filename.basename d.path
let directory d = Filename.dirname d.path
let extension d = Filename.extension d.path
let checksum d = d.checksum

let title d =
  let n = name d in
  match Filename.extension n with "" -> n | _ -> Filename.chop_extension n

