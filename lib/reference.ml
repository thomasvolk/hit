open Sexplib.Std

module type PrefixType = sig
  val prefix : string
end

let hash s = Digest.MD5.string s |> Digest.MD5.to_hex

module type IdType = sig
  type t = string [@@deriving sexp]

  val of_hash : string -> t
  val create : string -> t
  val prefix : string
  val hash : t -> string
  val to_string : t -> string
  val of_string : string -> t
  val compare : t -> t -> int
end

module Make (P : PrefixType) : IdType = struct
  type t = string [@@deriving sexp]

  exception InvalidHashInput of string

  let of_hash h =
    if String.length h = 32 then h
    else
      raise
        (InvalidHashInput
           ("invalid hash length: " ^ string_of_int (String.length h)))

  let create = function
    | "" -> raise (InvalidHashInput "can not hash an empty string")
    | s -> of_hash (hash s)

  let to_string s = P.prefix ^ "-" ^ s

  let of_string s =
    match String.split_on_char '-' s with
    | [ p; s ] when p = P.prefix && String.length s = 32 -> s
    | [ p; _ ] when p != P.prefix ->
        raise (InvalidHashInput ("invalid prefix: " ^ p))
    | [ _; s ] when String.length s != 32 ->
        raise
          (InvalidHashInput
             ("invalid hash length: " ^ string_of_int (String.length s)))
    | _ -> raise (InvalidHashInput s)

  let prefix = P.prefix
  let hash s = s
  let compare a b = String.compare (to_string a) (to_string b)
end
