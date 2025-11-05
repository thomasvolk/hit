open Sexplib.Std

module Checksum = struct
  type t = string [@@deriving sexp]

  let create cnt = Digest.MD5.string cnt |> Digest.MD5.to_hex
end

module DocumentId = struct
  let prefix = "doc"
end

module Id = Reference.Make (DocumentId)

module Meta = struct
  type t = { source : string; path : string; checksum : Checksum.t }
  [@@deriving sexp]

  let make_reference s p = s ^ "::" ^ p
  let create s p c = { source = s; path = p; checksum = c }
  let path m = m.path
  let name m = Filename.basename m.path
  let directory m = Filename.dirname m.path
  let extension m = Filename.extension m.path
  let title m = 
    let n = name m in
    match Filename.extension n with
      | "" -> n
      | _ -> Filename.chop_extension n
  let source m = m.source
  let reference m = make_reference m.source m.path
  let id m = Id.create (reference m)
  let checksum m = m.checksum
end

type t = { meta : Meta.t; content : string }

let create m c = { meta = m; content = c }

let from_source s p c =
  let m = Meta.create s p (Checksum.create c) in
  { meta = m; content = c }

let content d = d.content
let meta d = d.meta
let id d = Meta.id d.meta
let checksum d = d.meta.checksum

module DocumentMap = Map.Make (Id)
