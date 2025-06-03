
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
  add_path_sep "" (Reference.to_string r)


module type StorageType = sig
  type t
  type config

  val create : config -> t
  
  val load_doc_table : Reference.t -> t -> Index.DocumentTable.t

  val save_doc_table : Reference.t -> Index.DocumentTable.t -> t -> unit

  val load_token_table : t -> Index.TokenTable.t

  val save_token_table : Index.TokenTable.t -> t -> unit
end


module type StorageInstance = sig
  module Impl : StorageType
  val t : Impl.t
end


let storage (type a) (module S : StorageType with type config = a) config =
  (module struct
    module Impl = S
    let t = S.create config
  end : StorageInstance)


module FileStorage = struct
  type config = string
  type t = { 
    base_path : string;
  }

  let create path = { base_path = path }

  module Doc_table_file = struct
    let path conf = Filename.concat conf.base_path "doc-table"

    let position_list_to_string e = e |> List.map string_of_int |> String.concat " " |> String.trim

    let entry_to_string ti =
      let open Index.DocumentTable in
      let rec build el s = match el with
        | [] -> s
        | (r, e) :: rest -> 
             build rest (s ^ Reference.to_string r ^ " " ^ (position_list_to_string e ^ "\n"))
      in
      build (DocMap.to_list ti) ""

    let parse_row s =
      let rl = String.trim s
        |> String.split_on_char ' '
        |> List.filter (fun t -> String.length t > 0)
      in
      match rl with
      | [_] | [] -> None
      | ref :: pl -> Some(Reference.of_string ref, (List.map int_of_string pl))

    let load k conf = 
      let ti = Index.DocumentTable.empty in
      let filename = Filename.concat (path conf) (ref_to_path k) in
      if Sys.file_exists filename then
        let rec add_rows t rl = match rl with
          | [] -> t
          | r :: rest -> 
              let tu = match parse_row r with
                | Some((r, pl)) -> Index.DocumentTable.add r pl t
                | None -> t
              in
              add_rows tu rest
        in
        add_rows ti (String.split_on_char '\n' (read_file filename))
      else
        ti

    let save k ti conf =
      let filename = Filename.concat (path conf) (ref_to_path k) in
      write_file (entry_to_string ti) filename
  end

  module Term_table_file = struct
    let filename conf = Filename.concat conf.base_path "term-table"

    let load conf = 
      let tt = Index.TokenTable.empty in
      let f = filename conf in
      if Sys.file_exists f then
        let rec add_rows t = function
          | [] -> t
          | r :: rest -> 
              let tr = match String.split_on_char ' ' r with
                        | [term; dtref] -> Index.TokenTable.add term dtref t 
                        | _ -> t
          in
          add_rows tr rest
        in
        add_rows tt (String.split_on_char '\n' (read_file f))
      else
        tt

    let save tt conf =
      let rec entry_to_string s = function
        | [] -> s 
        | (term, dtref) :: rest -> 
          entry_to_string (s ^ term ^ " " ^ dtref ^ "\n" ) rest
      in
      let cnt = entry_to_string "" (Index.TokenTable.TokenMap.to_list tt) in
      let f = filename conf in
      write_file cnt f

  end

  let load_doc_table = Doc_table_file.load

  let save_doc_table = Doc_table_file.save

  let load_token_table = Term_table_file.load

  let save_token_table = Term_table_file.save

end

let file_storage path = storage (module FileStorage) path

