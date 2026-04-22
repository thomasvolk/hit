open Sexplib.Std

module UTF_8 = struct
  let cmap cmap s =
    let b = Buffer.create (String.length s * 2) in
    let add_map _ _ u =
      let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
      match cmap u with
      | `Self -> Uutf.Buffer.add_utf_8 b u
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
    in
    Uutf.String.fold_utf_8 add_map () s;
    Buffer.contents b

  let lowercase s = cmap Uucp.Case.Map.to_lower s
  let uppercase s = cmap Uucp.Case.Map.to_upper s
end

let id_prefix = "tkn"
let file_name = "token.hit"

module Id = Hash.Make (struct
  let prefix = id_prefix
end)

type t = string [@@deriving sexp]

let create w = w

let default_separators =
  " |()[]{}<>!'\"?=§$%&\\#*/+-_´`^@°:;,.~…»«≈" ^ String.make 1 '\160'
  |> String.to_seq |> List.of_seq

let from_string ?(token_start_char = 0x20) ?(separators = default_separators)
    ?(min_token_length = 2) s =
  let split s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      let current = String.unsafe_get s i in
      if current < Char.chr token_start_char || List.mem current separators then (
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i)
    done;
    String.sub s 0 !j :: !r
  in
  split s |> List.filter (fun s -> String.length s >= min_token_length)

let group tokens =
  let module StringMap = Map.Make (String) in
  tokens
  |> List.mapi (fun p t -> (t, p))
  |> List.rev
  |> List.fold_left
       (fun acc (t, p) ->
         let entry =
           match StringMap.find_opt t acc with
           | Some l -> p :: l
           | None -> [ p ]
         in
         StringMap.add t entry acc)
       StringMap.empty
  |> StringMap.to_list

module DocumentEntry : sig
  type t = private int list

  val of_list : int list -> t
  val to_list : t -> int list
  val compare : t -> t -> int
  val count : t -> int
  val t_of_sexp : Core.Sexp.t -> t
  val sexp_of_t : t -> Core.Sexp.t
end = struct
  type t = int list [@@deriving sexp]

  let of_list l = l
  let to_list l = l
  let compare a b = compare a b
  let count t = List.length t
end
