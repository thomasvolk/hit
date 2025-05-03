
module type Persistence = sig
  type t
  type config

  val load : string -> config -> t

  val save : t -> config -> unit
end

module Make (P : Persistence) = struct
  type t = P.t
  type config = P.config
  
  let load n c = P.load n c

  let save t c = P.save t c
end

let read_file filename = 
    let ic = In_channel.open_text filename in
    try
      let content = In_channel.input_all ic in
      In_channel.close ic;
      content
    with exn ->
      In_channel.close ic;
      raise exn

let rec create_dirs path =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir && Sys.is_directory dir) then begin
    create_dirs dir;
    Sys.mkdir dir 0o755;
  end

let write_file content filename =
  create_dirs filename;
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc content;
  Out_channel.close oc


module Path = struct

  type t = string

  let of_ref r =
    let folder_name_len = 2 in
    let folder_cnt = 4 in
    let hash_len = 32 in
    let rec add_path_sep p s = 
      let slen = String.length s in
      if slen <= (hash_len - (folder_cnt * folder_name_len))
      then p ^ s
      else
        let f = String.sub s 0 folder_name_len in
        let r = String.sub s folder_name_len (slen - folder_name_len) in
        add_path_sep (p ^ f ^ "/") r
    in
    add_path_sep "" (Ref.to_string r)

  let to_string t = t

end


module TermIndexFile = struct
  type t = Index.TermIndex.t

  type config = { 
    base_path : string;
  }

  let create path = { base_path = path }

  let index_path c = Filename.concat c.base_path "term-index"

  let entry_to_string e = 
    let open Index.TermIndex.Entry in
    let pl = positions e |> List.map string_of_int |> String.concat " "
    in
    ((Ref.to_string (ref e)) ^ " " ^ pl  |> String.trim)

  let term_index_to_string ti =
    let open Index.TermIndex in
    let rec build el s = match el with
      | [] -> s
      | (_, e) :: rest -> 
           build rest (s ^ (entry_to_string e ^ "\n"))
    in
    build (EntryMap.to_list ti.entries) ""

  let entry_of_string s =
    let open Index.TermIndex.Entry in
    let rl = String.trim s
      |> String.split_on_char ' '
      |> List.filter (fun t -> String.length t > 0)
    in
    match rl with
    | [_] | [] -> None
    | ref :: pl -> Some( create (Ref.of_string ref)  (List.map int_of_string pl))

  let load w c = 
    let open Util in
    let filename = Filename.concat (index_path c) (Hash.create w |> Hash.to_path) in
    if Sys.file_exists filename then
      Term.of_string (read_file filename)
    else
      Term.empty 

  let save r c =
    let open Util in
    let filename = Filename.concat (index_path c) (Path.of_ref (Index.TermIndex.ref r)) in
    write_file (term_index_to_string r) filename
end
