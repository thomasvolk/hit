
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


let ref_to_path r =
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


module type StorageType = sig
  type t
  type config

  val create : config -> t
  
  val load_doc_table : Ref.t -> t -> Doc_table.t

  val save_doc_table : Ref.t -> Doc_table.t -> t -> unit
end


module type StorageInstance = sig
  module Impl : StorageType
  val t : Impl.t
end


let doc_table_storage (type a) (module S : StorageType with type config = a) config =
  (module struct
    module Impl = S
    let t = S.create config
  end : StorageInstance)


module Doc_table_file = struct
  type config = string
  type t = { 
    base_path : string;
  }

  let create path = { base_path = path }

  let path conf = Filename.concat conf.base_path "doc-table"

  let position_list_to_string e = e |> List.map string_of_int |> String.concat " " |> String.trim

  let entry_to_string ti =
    let open Doc_table in
    let rec build el s = match el with
      | [] -> s
      | (r, e) :: rest -> 
           build rest (s ^ Ref.to_string r ^ " " ^ (position_list_to_string e ^ "\n"))
    in
    build (DocMap.to_list ti) ""

  let parse_row s =
    let rl = String.trim s
      |> String.split_on_char ' '
      |> List.filter (fun t -> String.length t > 0)
    in
    match rl with
    | [_] | [] -> None
    | ref :: pl -> Some(Ref.of_string ref, (List.map int_of_string pl))

  let load_doc_table k conf = 
    let ti = Doc_table.empty in
    let filename = Filename.concat (path conf) (ref_to_path k) in
    if Sys.file_exists filename then
      let rec add_rows t rl = match rl with
        | [] -> t
        | r :: rest -> 
            let tu = match parse_row r with
              | Some((r, pl)) -> Doc_table.add r pl t
              | None -> t
            in
            add_rows tu rest
      in
      add_rows ti (String.split_on_char '\n' (read_file filename))
    else
      ti

  let save_doc_table k ti conf =
    let filename = Filename.concat (path conf) (ref_to_path k) in
    write_file (entry_to_string ti) filename
end

let doc_table_file_storage path = doc_table_storage (module  Doc_table_file) path

