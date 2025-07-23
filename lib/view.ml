module Highlight = struct
  type part = Text of string | Token of Text.Token.t

  module Line = struct
    type t = { number : int; parts : part list }

    let create n pl = { number = n; parts = pl }
    let number l = l.number
    let parts l = l.parts
  end

  type t = Line.t list

  let lines d =
    let rec loop r c pos = function
      | [] -> r
      | l :: tl ->
          let r' = (c, pos, l) :: r in
          loop r' (c + 1) (pos + String.length l + 1) tl
    in
    loop [] 1 0 (String.split_on_char '\n' (Table.Document.content d))

  let highlight_line l pos pl =
    let rec loop pos r l = function
      | [] -> r @ [ Text l ]
      | (f, t) :: rest ->
          let token_len = t - f in
          let rf = f - pos in
          let r' = r @ [ Text (String.sub l 0 rf) ] in
          let r'' = r' @ [ Token (String.sub l rf token_len) ] in
          let rt = t - pos in
          let rest_len = String.length l - rt in
          let l' = String.sub l rt rest_len in
          let pos' = t in
          loop pos' r'' l' rest
    in
    loop pos [] l pl

  let create doc tl =
    let to_highlight pos l tl =
      let f, t = (pos, pos + String.length l) in
      tl
      |> List.map (Text.TokenEntry.in_range f t)
      |> List.flatten
      |> List.sort (fun (a, _) (b, _) -> a - b)
    in
    let rec collect r tl = function
      | [] -> r
      | (c, pos, l) :: rest ->
          let r' =
            match to_highlight pos l tl with
            | [] -> r
            | pl -> Line.create c (highlight_line l pos pl) :: r
          in
          collect r' tl rest
    in
    collect [] tl (lines doc)
end
