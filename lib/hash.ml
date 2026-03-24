open Sexplib.Std

module type PrefixType = sig
  val prefix : string
end

module type IdType = sig
  type t
  val create : string -> t
  val t_of_sexp : Core.Sexp.t -> t
  val sexp_of_t : t -> Core.Sexp.t
  val to_string : t -> string
  val from_string : string -> t
  val to_path : ?folder_cnt:int -> ?folder_name_len:int -> t -> string
  val from_path : string -> t
  val compare : t -> t -> int
end

exception InvalidHashInput of string

module Make (P : PrefixType) : IdType = struct
  type t = string * string [@@deriving sexp]

  let create c = (P.prefix, Digest.MD5.string c |> Digest.MD5.to_hex)

  let from_string s =
    match String.split_on_char '-' s with
    | [ p; v ] -> (p, v)
    | _ -> raise (InvalidHashInput s)

  let to_string (p, v) = Format.sprintf "%s-%s" p v
  let sexp_of_t t = Core.Sexp.Atom (to_string t)
  let t_of_sexp se = from_string (Core.Sexp.t_of_sexp se |> Core.Sexp.to_string)

  let to_path ?(folder_cnt = 4) ?(folder_name_len = 2) (p, v) =
    let hex_hash_len = Digest.MD5.hash_length * 2 in
    let rec add_path_sep p s =
      let slen = String.length s in
      if slen <= hex_hash_len - (folder_cnt * folder_name_len) then p ^ s
      else
        let f = String.sub s 0 folder_name_len in
        let r = String.sub s folder_name_len (slen - folder_name_len) in
        add_path_sep (p ^ f ^ Filename.dir_sep) r
    in
    p ^ Filename.dir_sep ^ add_path_sep "" v

  let from_path path =
    let rec split h p =
      let d = Filename.dirname p and b = Filename.basename p in
      if d = "." then b :: h else split (b :: h) d
    in
    match split [] path with
    | p :: rest -> (p, List.fold_left ( ^ ) "" rest)
    | _ -> raise (InvalidHashInput path)

  let compare a b = String.compare (to_string a) (to_string b)
end
