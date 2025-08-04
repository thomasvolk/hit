open Sexplib.Std

module IndexConfig = struct
  type t = { max_token_count : int } [@@deriving sexp]

  let create ?(max_token_count = 30000) () = { max_token_count }
  let max_token_count t = t.max_token_count
end
