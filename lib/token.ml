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


let separators = String.to_seq " \t()[]{}<>!'\"?=§$%&\\#*/+-_´`^@°:;,." |> List.of_seq

let parse doc = 
  let split sep (s, r, c) =
    let rec next c tl row = match row with
      | w :: rt -> next (c + (String.length w) + 1) (tl @ [(w, r, c)]) rt
      | [] -> tl
    in
    next c [] (String.split_on_char sep s)
    
  in
  let rec split_items sl l =
    match sl with
    | s :: rsl -> split_items rsl (List.map (split s) l |> List.flatten)
    | [] -> l
  in
  let is_not_empty (w, _, _) = String.length w > 0
  in
  String.split_on_char '\n' doc
  |> List.mapi (fun i w -> w, i, 0)
  |> split_items separators
  |> List.filter is_not_empty
  |> List.map (fun (w, r, c) -> create w r c )
