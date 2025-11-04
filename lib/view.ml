open Index
open Text

module Preview = struct
  type part = Text of string | Token of Text.Token.t
  type t = part list

  let of_tokens cnt tokens =
    let rec parse r c s = function
      | [] -> r @ [ Text s ]
      | (token, pos) :: rest ->
          let text_len = pos - c in
          let txt = String.sub s 0 text_len in
          let token_len = String.length token in
          let cut = text_len + token_len in
          let s' = String.sub s cut (String.length s - cut) in
          let r' = r @ [ Text txt; Token token ] in
          let c' = pos + token_len in
          parse r' c' s' rest
    in
    parse [] 0 cnt tokens

  let create doc sr =
    let cd_tokens =
      SearchResult.closest_distances sr
      |> List.map (fun m -> [ TokenPair.st m; TokenPair.en m ])
      |> List.flatten
      |> List.sort_uniq (fun a b -> snd a - snd b)
    in
    let tokens =
      match cd_tokens with
      | [] ->
          let tel = SearchResult.token_entries sr in
          let te = List.hd tel in
          let p = List.hd (Text.TokenEntry.positions te) in
          let t = Text.TokenEntry.token te in
          [ (t, p) ]
      | _ -> cd_tokens
    in
    let cnt = Document.content doc in
    of_tokens cnt tokens

  let shorten_txt ?(max_len = 60) txt =
    if String.length txt > max_len then
      let pl = max_len / 3 in
      let tl = String.length txt in
      String.sub txt 0 pl ^ " ... " ^ String.sub txt (tl - pl) pl
    else txt

  let shorten ?(max_len = 60) pr =
    let rec loop r = function
      | [] -> r
      | Text t :: rest -> loop (r @ [ Text (shorten_txt ~max_len t) ]) rest
      | t :: rest -> loop (r @ [ t ]) rest
    in
    loop [] pr
end
