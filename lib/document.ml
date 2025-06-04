open Sexplib.Std

module Id = struct
  include Reference
end

module Meta = struct
  type t = {
    source: string;
    path: string;
  } [@@deriving sexp]
end

type t = {
  id: Id.t;
  meta: Meta.t;
  content: string;
}

let create r m c = {
  id=r;
  meta=m;
  content=c
}

let id d = d.id

let content d = d.content

let meta d = d.meta
