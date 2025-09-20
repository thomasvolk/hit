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

let contains lc c =
    let rec contains_loop i c = function
      | true -> true
      | false when i >= String.length lc -> false
      | _ -> contains_loop (i + 1) c (lc.[i] = c)
    in
    contains_loop 0 c false

module Parser = struct
  module TokenMap = Map.Make (String)

  let split_except allowed s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if (not (contains allowed (String.unsafe_get s i))) then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r

  let parse allowed_token_chars s =
    let split allowed_chars (s, c) =
      let rec next c tl row =
        match row with
        | w :: rt -> next (c + String.length w + 1) (tl @ [ (w, c) ]) rt
        | [] -> tl
      in
      next c [] (split_except allowed_chars s)
    in
    let tokenize allowed_chars l =
      List.map (split allowed_chars) l |> List.flatten
    in
    let is_not_empty (w, _) = String.length w > 0 in
    let consolidate wl =
      let rec map wl tm =
        match wl with
        | [] -> tm
        | (w, p) :: rl ->
            let tm =
              TokenMap.update w
                (fun pl ->
                  match pl with Some pl -> Some (p :: pl) | None -> Some [ p ])
                tm
            in
            map rl tm
      in
      map wl TokenMap.empty |> TokenMap.to_list
    in
    [ (s, 0) ]
    |> tokenize allowed_token_chars |> List.filter is_not_empty
    |> List.map (fun (w, c) -> (String.lowercase_ascii w, c))
    |> consolidate
    |> List.map (fun (w, c) -> TokenEntry.create w c)
end
