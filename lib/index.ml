type t = {
  path: string;
}

module Document = struct
  type t = {
    path: string;
    origin: string;
  }

  let source t = t.origin ^ "::" ^ t.path

  let ref t = Digest.MD5.to_hex (source t)

  let name t = Filename.basename t.path

  let create p o = {
    path = p;
    origin = o
  }
end

module Register = struct
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

  let ref_hash w = Digest.MD5.to_hex w

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
end

let register_path t = Filename.concat t.path "entry"

let open_register w t = 
  let filename = Filename.concat (register_path t) (Register.ref_hash w) in
  if Sys.file_exists filename then
   Register.of_string (Io.read_file filename)
  else
   Register.empty 

let store_register w r t =
  let filename = Filename.concat (register_path t) (Register.ref_hash w) in
  Io.write_file (Register.to_string r) filename
