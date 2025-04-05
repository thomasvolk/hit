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

  let create = RefMap.empty

  let add t r = RefMap.add (fst r) r t

  let of_string s =
    let t = create in
    let parse_row r = match String.split_on_char ' ' r with
      | ref :: pl -> Some((ref, List.map int_of_string pl))
      | [] -> None
    in
    let rec add_rows = function
      | [] -> t
      | r :: rest -> match parse_row r with
        | Some(r) -> add t r
        | None -> ();
        add_rows rest
    in
    add_rows (String.split_on_char '\n' s)

end

let entry_path t = Filename.concat t.path "entry"

let entry t w = 
  let filename = Filename.concat (entry_path t) w in
  if Sys.file_exists filename then
   Entry.of_string (Io.read_file filename)
  else
   Entry.create 
