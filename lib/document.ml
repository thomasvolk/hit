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
