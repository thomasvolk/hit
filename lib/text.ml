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

let min_of_list l =
  let sorted = List.sort (fun a b -> a - b) l in
  List.nth_opt sorted 0

module TokenEntry = struct
  type t = { token : Token.t; positions : Token.Pos.t list } [@@deriving sexp]

  let create t p = { token = t; positions = p }
  let token e = e.token
  let positions e = e.positions
  let count e = List.length e.positions
  let has_positions e = count e > 0

  let in_range f t e =
    e.positions |> List.map Token.Pos.to_int
    |> List.map (fun p -> (p, p + Token.length e.token))
    |> List.filter (fun (pf, pt) -> pf >= f && pt <= t)

  let closest_distance e o =
    let rec closest_to d pl p =
      match pl with
      | [] -> ( match d with None -> 0 | Some n -> n)
      | c :: r ->
          let nd = Int.abs (c - p) in
          let d' =
            match d with None -> nd | Some n -> if nd < n then nd else n
          in
          closest_to (Some d') r p
    in
    List.map (closest_to None o.positions) e.positions |> min_of_list
end

module Parser = struct
  module TokenMap = Map.Make (String)

  let separators =
    String.to_seq "\r\n \t|()[]{}<>!'\"?=§$%&\\#*/+-_´`^@°:;,.~" |> List.of_seq

  let parse s =
    let split sep (s, c) =
      let rec next c tl row =
        match row with
        | w :: rt -> next (c + String.length w + 1) (tl @ [ (w, c) ]) rt
        | [] -> tl
      in
      next c [] (String.split_on_char sep s)
    in
    let rec tokenize sl l =
      match sl with
      | s :: rsl -> tokenize rsl (List.map (split s) l |> List.flatten)
      | [] -> l
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
    |> tokenize separators |> List.filter is_not_empty
    |> List.map (fun (w, c) -> (String.lowercase_ascii w, c))
    |> consolidate
    |> List.map (fun (w, c) -> TokenEntry.create w c)
end
