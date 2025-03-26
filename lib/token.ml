type t = {
  token: string;
  row: int;
  col: int;
}

let create t r c = {
  token = t;
  row = r;
  col = c;
}

let is_not_empty t = String.length t.token > 0

let parse doc = 
  let parse_row r s =
    let rec next c tl row = match row with
      | w :: rt -> next (c + (String.length w) + 1) (tl @ [create w r c]) rt
      | [] -> tl
    in
    next 0 [] (String.split_on_char ' ' s)
    
  in
  String.split_on_char '\n' doc
  |> List.mapi (fun r e -> (r, e))
  |> List.map (fun (r, e) -> parse_row r e)
  |> List.flatten
  |> List.filter is_not_empty
