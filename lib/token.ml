type t = {
  token: string;
  pos: int;
}

let create t p = {
  token = t;
  pos = p;
}


let separators = String.to_seq "\r\n \t|()[]{}<>!'\"?=§$%&\\#*/+-_´`^@°:;,." |> List.of_seq

let parse doc = 
  let split sep (s, c) =
    let rec next c tl row = match row with
      | w :: rt -> next (c + (String.length w) + 1) (tl @ [(w, c)]) rt
      | [] -> tl
    in
    next c [] (String.split_on_char sep s)
    
  in
  let rec split_items sl l =
    match sl with
    | s :: rsl -> split_items rsl (List.map (split s) l |> List.flatten)
    | [] -> l
  in
  let is_not_empty (w, _) = String.length w > 0
  in
  [(doc, 0)]
  |> split_items separators
  |> List.filter is_not_empty
  |> List.map (fun (w, c) -> String.lowercase_ascii w, c)
  |> List.map (fun (w, c) -> create w c)
