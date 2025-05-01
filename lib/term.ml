module EntryMap = Map.Make(String)

module Entry = struct
  type t = string * int list

  exception InvalidRef of string

  let doc_id t = fst t

  let positions t = snd t

  let to_string t = 
    let pl = positions t |> List.map string_of_int |> String.concat " "
    in
    ((doc_id t) ^ " " ^ pl  |> String.trim)

  let of_string s =
    let rl = String.trim s
      |> String.split_on_char ' '
      |> List.filter (fun t -> String.length t > 0)
    in
    match rl with
    | [_] | [] -> None
    | ref :: pl -> Some((ref, List.map int_of_string pl))

  let create d pl = 
    if List.length pl > 0 then
      (d, pl)
    else
      raise (InvalidRef "position list is empty")
end

type t = Entry.t EntryMap.t

let empty = EntryMap.empty

let add r t = EntryMap.add (Entry.doc_id r) r t

let of_string s =
  let rec add_rows t rl = match rl with
    | [] -> t
    | r :: rest -> 
        let tu = match Entry.of_string r with
          | Some(r) -> add r t
          | None -> t
        in
        add_rows tu rest
  in
  add_rows empty (String.split_on_char '\n' s)

let to_string t =
  let rec build el s = match el with
    | [] -> s
    | (_, e) :: rest -> 
         build rest (s ^ (Entry.to_string e ^ "\n"))
  in
  build (EntryMap.to_list t) ""

let size t = EntryMap.cardinal t
