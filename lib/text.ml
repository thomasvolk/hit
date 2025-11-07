open Sexplib.Std

module Token = struct
  (* A Token is a word that can be searched *)

  type t = string [@@deriving sexp]

  let length t = String.length t

  module Pos = struct
    type t = int [@@deriving sexp]

    let to_int p = p
  end

  let to_string t = t
  let compare a b = String.compare a b
end

module TokenPair = struct
  type t = (Token.t * int) * (Token.t * int)

  let create ft fp tt tp = ((ft, fp), (tt, tp))
  let st d = fst d
  let en d = snd d
  let distance_vec d = snd (en d) - snd (st d)
  let distance d = Int.abs (distance_vec d)
end

module TokenEntry = struct
  module Flags = struct
    type t = { title : bool; directory : bool; extension : bool; source : bool }
    [@@deriving sexp]

    let empty =
      { title = false; directory = false; extension = false; source = false }

    let create t d e s = { title = t; directory = d; extension = e; source = s }
    let create_title = create true false false false
    let create_directory = create false true false false
    let create_extension = create false false true false
    let create_source = create false false false true 
    let set_title f = { f with title = true }
    let set_directory f = { f with directory = true }
    let set_extension f = { f with extension = true }
    let set_source f = { f with source = true }
  end

  type t = { token : Token.t; positions : Token.Pos.t list; flags : Flags.t }
  [@@deriving sexp]

  let create t p f = { token = t; positions = p; flags = f }
  let create_title t p = { token = t; positions = p; flags = Flags.create_title }
  let create_directory t p = { token = t; positions = p; flags = Flags.create_directory }
  let create_extension t p = { token = t; positions = p; flags = Flags.create_extension }
  let create_source t p = { token = t; positions = p; flags = Flags.create_source }
  let token e = e.token
  let token_length e = Token.length e.token
  let positions e = e.positions
  let count e = List.length e.positions
  let has_positions e = count e > 0
  let add_position p e = { e with positions = [p] @ e.positions |> List.sort (-) }
  let flags e = e.flags
  let set_title e = { e with flags = Flags.set_title e.flags }
  let set_directory e = { e with flags = Flags.set_directory e.flags }
  let set_extension e = { e with flags = Flags.set_extension e.flags }
  let set_source e = { e with flags = Flags.set_source e.flags }

  let in_range f t e =
    e.positions |> List.map Token.Pos.to_int
    |> List.map (fun p -> (p, p + Token.length e.token))
    |> List.filter (fun (pf, pt) -> pf >= f && pt <= t)

  let closest_distance e o =
    let open TokenPair in
    let rec closest_to d opl ep =
      match opl with
      | [] -> d
      | op :: r ->
          let nd = create o.token op e.token ep in
          let d' =
            match d with
            | None -> nd
            | Some n -> if distance nd < distance n then nd else n
          in
          closest_to (Some d') r ep
    in
    match (e.positions, o.positions) with
    | [], _ -> None
    | _, [] -> None
    | ep, op ->
        let sorted =
          List.map (closest_to None op) ep
          |> List.filter Option.is_some |> List.map Option.get
          |> List.sort (fun a b -> distance a - distance b)
        in
        List.nth_opt sorted 0
end

let split_on_control_chars s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i < Char.chr 0x20 then (
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i)
  done;
  String.sub s 0 !j :: !r

module Parser = struct
  module TokenMap = Map.Make (String)

  let get_tokens separators ?(min_token_length = 2) s =
    let split split_func (s, c) =
      let rec next c tl = function
        | w :: rt -> next (c + String.length w + 1) (tl @ [ (w, c) ]) rt
        | [] -> tl
      in
      next c [] (split_func s)
    in
    let rec tokenize separators l =
      let l' = List.map (split split_on_control_chars) l |> List.flatten in
      match separators with
      | s :: separators' ->
          tokenize separators'
            (List.map (split (String.split_on_char s)) l' |> List.flatten)
      | [] -> l
    in
    let is_not_empty (w, _) = String.length w >= min_token_length in
    [ (s, 0) ]
    |> tokenize separators |> List.filter is_not_empty
    |> List.map (fun (w, c) -> (String.lowercase_ascii w, c))

  let parse separators ?(min_token_length = 2) doc =
    let rec consolidate tl create update tm =
      match tl with
        | [] -> tm
        | (w, p) :: rest -> 
            let tm' =
              TokenMap.update w
                (fun te ->
                  match te with
                    | Some te -> Some (update te w p)
                    | None -> Some (create w p))
                tm
            in
            consolidate rest create update tm'
    in
    let tokens = get_tokens separators ~min_token_length 
    and meta = Document.meta doc
    in
    let title_tokens = tokens (Document.Meta.title meta)
    and dir_tokens = tokens (Document.Meta.directory meta)
    and ext_tokens = tokens (Document.Meta.extension meta)
    and src_tokens = tokens (Document.Meta.source meta)
    and content_tokens = tokens (Document.content doc)
    in
    TokenMap.empty
    |> consolidate title_tokens
      (fun w _ -> TokenEntry.create_title w [])
      (fun te _ _ -> TokenEntry.set_title te)
    |> consolidate dir_tokens
      (fun w _ -> TokenEntry.create_directory w [])
      (fun te _ _ -> TokenEntry.set_directory te)
    |> consolidate ext_tokens
      (fun w _ -> TokenEntry.create_extension w [])
      (fun te _ _ -> TokenEntry.set_extension te)
    |> consolidate src_tokens
      (fun w _ -> TokenEntry.create_source w [])
      (fun te _ _ -> TokenEntry.set_source te)
    |> consolidate content_tokens
      (fun w p -> TokenEntry.create w [p] TokenEntry.Flags.empty)
      (fun te _ p -> TokenEntry.add_position p te)
    |> TokenMap.to_list |> List.map snd
end
