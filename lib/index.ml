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

  let of_string _s = create  (* FIXME: this is wrong! *)

  let add t r = RefMap.add (fst r) r t
end

let entry_path t = Filename.concat t.path "entry"

let entry t w = 
  let filename = Filename.concat (entry_path t) w in
  match Io.read_file filename with
   | Some(content) -> Entry.of_string content
   | None -> Entry.create 
