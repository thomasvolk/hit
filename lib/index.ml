module EntryMap = Map.Make(String)

module Entry = struct
  type t = string * int list

  exception InvalidRef of string

  let doc_id t = fst t

  let positions t = snd t

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
  let parse_row r =
    let rl = String.trim r
      |> String.split_on_char ' '
      |> List.filter (fun s -> String.length s > 0)
    in
    match rl with
    | [_] | [] -> None
    | ref :: pl -> Some((ref, List.map int_of_string pl))
  in
  let rec add_rows t rl = match rl with
    | [] -> t
    | r :: rest -> 
        let tu = match parse_row r with
          | Some(r) -> add r t
          | None -> t
        in
        add_rows tu rest
  in
  add_rows empty (String.split_on_char '\n' s)

let to_string t =
  let build_row ref = 
    let pl = Entry.positions ref |> List.map string_of_int |> String.concat " "
    in
    ((Entry.doc_id ref) ^ " " ^ pl  |> String.trim) ^ "\n"
  in
  let rec build el s = match el with
    | [] -> s
    | (_, ref) :: rest -> 
         build rest (s ^ (build_row ref))
  in
  build (EntryMap.to_list t) ""

let size t = EntryMap.cardinal t

module FileIo = struct
  type r = t

  let path = "index"

  let register_path = Filename.concat path "register"

  let load w = 
    let open Util in
    let filename = Filename.concat register_path (Hash.create w |> Hash.to_path) in
    if Sys.file_exists filename then
      of_string (Io.read_file filename)
    else
      empty 

  let save w r =
    let open Util in
    let filename = Filename.concat register_path (Hash.create w |> Hash.to_path) in
    Io.write_file (to_string r) filename
end
