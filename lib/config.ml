open Sexplib.Std

let default_separators =
  " |()[]{}<>!'\"?=§$%&\\#*/+-_´`^@°:;,.~…»«≈" ^ String.make 1 '\160'

module IndexConfig = struct
  type t = {
    max_token_count : int;
    token_separators : string;
    min_token_length : int;
  }
  [@@deriving sexp]

  let create ?(max_token_count = 30000) ?(token_separators = default_separators)
      ?(min_token_length = 2) () =
    { max_token_count; token_separators; min_token_length }

  let max_token_count t = t.max_token_count
  let token_separators t = t.token_separators
  let token_separators_seq t = String.to_seq t.token_separators |> List.of_seq
  let min_token_length t = t.min_token_length
end
