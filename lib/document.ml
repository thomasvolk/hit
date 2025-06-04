open Sexplib.Std

module Id = struct
  include Reference
end

module Meta = struct
  type t = {
    source: string;
    path: string;
  } [@@deriving sexp]

  let create s p = { source=s; path=p; }

  let path m = m.path

  let source m = m.source

  let id m = m.source ^ "::" ^ m.path
end

type t = {
  id: Id.t;
  meta: Meta.t;
  content: string;
}

let create m c = {
  id=(Id.create (Meta.id m));
  meta=m;
  content=c
}

let id d = d.id

let content d = d.content

let meta d = d.meta
