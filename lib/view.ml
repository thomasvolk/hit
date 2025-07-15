
module Highlight = struct
  type part = Text of string | Token of Text.Token.t
  module Line = struct
    type t = {
      number: int;
      parts: part list;
    }
    let create n pl = { number=n; parts=pl }
    let number l = l.number
    let parts l = l.parts
  end
  type t = Line.t list

  let lines d = 
    let rec loop r c pos = function
      | [] -> r
      | l :: tl -> 
          let r' = (c, pos, l) :: r in
          loop r' (c + 1) ((pos + (String.length l)) + 1) tl
    in
    loop [] 1 0 (String.split_on_char '\n' (Table.Document.content d))

  let create doc tl =
    let to_highlight pos l tl =
      let f, t = pos, (pos + (String.length l)) in
      tl |> List.map (Text.TokenEntry.in_range f t) |> List.flatten
    in
    let rec collect r tl = function
      | [] -> r
      | (c, pos, l) :: rest ->
          let r' = match to_highlight pos l tl with
            | [] -> r
            | _ -> (Line.create c [Text(l)]):: r
          in
          collect r' tl rest
    in
    collect [] tl (lines doc)
    
end
