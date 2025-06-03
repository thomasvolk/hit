
type t = string

exception InvalidHashInput of string

let create = function 
  | "" -> raise (InvalidHashInput "can not hash an empty string")
  | s -> Digest.MD5.string s |> Digest.MD5.to_hex

let to_string t = t

let of_string s = Digest.MD5.of_hex s

let compare a b = String.compare a b
