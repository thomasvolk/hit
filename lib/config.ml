open Sexplib.Std

module IndexConfig = struct
  type t = {
     max_token_count: int
  } [@@deriving sexp]
end
