open Sexplib.Std

let default_token_chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYabcdefghijklmnopqrstuvwxyÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöøùúûüý"

module IndexConfig = struct
  type t = { max_token_count : int; token_chars : string } [@@deriving sexp]

  let create ?(max_token_count = 30000) ?(token_chars = default_token_chars) () = {
      max_token_count=max_token_count;
      token_chars=token_chars
    }
  let max_token_count t = t.max_token_count
  let token_chars t = t.token_chars
end
