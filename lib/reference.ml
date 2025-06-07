open Sexplib.Std

module type PrefixType = sig
  val prefix : string
end

module type IdType = sig
  type t = string * string [@@deriving sexp]
  val create : string -> t
  val prefix: t -> string
  val hash : t -> string
  val to_string : t -> string
  val of_string : string -> t
  val compare : t -> t -> int
end

module Make (P: PrefixType) : IdType = struct
  type t = string * string [@@deriving sexp]

  exception InvalidHashInput of string

  let create = function 
    | "" -> raise (InvalidHashInput "can not hash an empty string")
    | s -> (P.prefix, Digest.MD5.string s |> Digest.MD5.to_hex)

  let to_string (p, s) = p ^ "-" ^ s

  let of_string s = 
    match String.split_on_char '-' s with
    | [p; s] when p = P.prefix && String.length s = 32 -> (p, s)
    | [p; _] when p != P.prefix-> raise (InvalidHashInput ("invalid prefix: " ^ p))
    | [_; s] when String.length s = 32 -> raise (InvalidHashInput ("invalid hash length: " ^ (string_of_int (String.length s))))
    | _ -> raise (InvalidHashInput s)

  let prefix (p, _) = p

  let hash (_, s) = s

  let compare a b = String.compare (to_string a) (to_string b)

end
