
let split s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    let current = String.unsafe_get s i in
    if current < Char.chr 0x21 then (
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i)
  done;
  String.sub s 0 !j :: !r

let get_tokens ?(min_token_length = 2) s =
  let is_not_empty (w, _) = String.length w >= min_token_length in
  split s
  |> List.fold_left
       (fun (l, c) t -> (List.append [ (t, c) ] l, c + 1 + String.length t))
       ([], 0)
  |> fst |> List.rev |> List.filter is_not_empty
  |> List.map (fun (w, c) -> (String.lowercase_ascii w, c))

