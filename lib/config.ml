open Sexplib.Std

module IndexConfig = struct
  type t = {
    max_token_count : int;
    min_token_length : int;
  }
  [@@deriving sexp]

  let create ?(max_token_count = 30000)
      ?(min_token_length = 2) () =
    { max_token_count; min_token_length }

  let max_token_count t = t.max_token_count
  let min_token_length t = t.min_token_length
end
