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

module Entry = struct
  module RefMap = Map.Make(String)

  module Ref = struct
    type t = string * int list
  end

  type t = Ref.t RefMap.t

  let empty = RefMap.empty

  let add r t = RefMap.add (fst r) r t

  let of_string s =
    let parse_row r =
      let rl = String.trim r
        |> String.split_on_char ' '
        |> List.filter (fun s -> String.length s > 0)
      in
      match rl with
      | ref :: pl -> Some((ref, List.map int_of_string pl))
      | [] -> None
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
      let pl = snd ref |> List.map string_of_int |> String.concat " "
      in
      ((fst ref) ^ " " ^ pl  |> String.trim) ^ "\n"
    in
    let rec build el s = match el with
      | [] -> s
      | (_, ref) :: rest -> 
           build rest (s ^ (build_row ref))
    in
    build (RefMap.to_list t) ""

  let size t = RefMap.cardinal t
end

let entry_path t = Filename.concat t.path "entry"

let entry t w = 
  let filename = Filename.concat (entry_path t) w in
  if Sys.file_exists filename then
   Entry.of_string (Io.read_file filename)
  else
   Entry.empty 
