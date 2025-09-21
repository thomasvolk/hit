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
  type t = { token : Token.t; positions : Token.Pos.t list } [@@deriving sexp]

  let create t p = { token = t; positions = p }
  let token e = e.token
  let token_length e = Token.length e.token
  let positions e = e.positions
  let count e = List.length e.positions
  let has_positions e = count e > 0

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
    if String.unsafe_get s i < (Char.chr 0x1f) then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

module Parser = struct
  module TokenMap = Map.Make (String)

  let parse separators ?(min_token_length = 2) s =
    let split split_func (s, c) =
      let rec next c tl = function
        | w :: rt -> next (c + String.length w + 1) (tl @ [ (w, c) ]) rt
        | [] -> tl
      in
      next c [] (split_func s)
    in
    let rec tokenize separators l =
      let l' = List.map (split split_on_control_chars)  l |> List.flatten in
      match separators with
      | s :: separators' -> tokenize separators' (List.map (split (String.split_on_char s)) l' |> List.flatten)
      | [] -> l
    in
    let is_not_empty (w, _) = String.length w > 0 in
    let consolidate wl =
      let rec map wl tm =
        match wl with
        | [] -> tm
        | (w, _) :: rl when String.length w < min_token_length -> map rl tm
        | (w, p) :: rl ->
            let tm' =
              TokenMap.update w
                (fun pl ->
                  match pl with Some pl -> Some (p :: pl) | None -> Some [ p ])
                tm
            in
            map rl tm'
      in
      map wl TokenMap.empty |> TokenMap.to_list
    in
    [ (s, 0) ]
    |> tokenize separators |> List.filter is_not_empty
    |> List.map (fun (w, c) -> (String.lowercase_ascii w, c))
    |> consolidate
    |> List.map (fun (w, c) -> TokenEntry.create w c)
end
