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
  let split sep (s, r, c) =
    let rec next c tl row = match row with
      | w :: rt -> next (c + (String.length w) + 1) (tl @ [(w, r, c)]) rt
      | [] -> tl
    in
    next c [] (String.split_on_char sep s)
    
  in
  String.split_on_char '\n' doc
  |> List.mapi (fun i w -> (w, i, 0))
  |> List.map (split ' ')
  |> List.flatten
  |> List.map (split '\t')
  |> List.flatten
  |> List.map (fun (w, r, c) -> create w r c )
  |> List.filter is_not_empty
