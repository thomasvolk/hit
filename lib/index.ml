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

  exception InvalidRegister of string

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

  type t = {
    word: string;
    entries: Entry.t EntryMap.t
  }

  let word t = t.word

  let empty w = {
    word = w;
    entries = EntryMap.empty
  }

  let add r t = {
    word = t.word;
    entries = EntryMap.add (Entry.doc_id r) r t.entries
  }

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
    match String.split_on_char '\n' s with
      | (word :: rows) when String.length word > 0 -> add_rows (empty word) rows
      | _ -> raise (InvalidRegister ("invalid register: " ^ s))

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
    t.word ^ "\n" ^ build (EntryMap.to_list t.entries) ""

  let size t = EntryMap.cardinal t.entries
end

let register_path t = Filename.concat t.path "entry"

let open_register w t = 
  let filename = Filename.concat (register_path t) w in
  if Sys.file_exists filename then
   Register.of_string (Io.read_file filename)
  else
   Register.empty w

let store_register r t =
  let filename = Filename.concat (register_path t) (Register.word r) in
  Io.write_file (Register.to_string r) filename
