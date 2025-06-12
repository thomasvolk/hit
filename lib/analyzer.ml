open Sexplib.Std

module Entry = struct
  type t = { token : Token.t; positions : Token.Pos.t list } [@@deriving sexp]

  let create t p = { token = t; positions = p }
  let token e = e.token
  let positions e = e.positions
end

module Parser = struct
  module TokenMap = Map.Make (String)

  let separators =
    String.to_seq "\r\n \t|()[]{}<>!'\"?=§$%&\\#*/+-_´`^@°:;,." |> List.of_seq

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
    |> List.map (fun (w, c) -> Entry.create w c)
end
