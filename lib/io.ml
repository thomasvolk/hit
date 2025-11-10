open Table

let read_file filename =
  Logs.debug (fun m -> m "Read file: %s" filename);
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
  if not (Sys.file_exists dir && Sys.is_directory dir) then (
    create_dirs dir;
    Sys.mkdir dir 0o755)

let write_file content filename =
  Logs.debug (fun m -> m "Write file: %s" filename);
  create_dirs filename;
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc content;
  Out_channel.close oc

let write_file_with_producer p filename =
  Logs.debug (fun m -> m "Write file: %s" filename);
  create_dirs filename;
  let oc = Out_channel.open_text filename in
  let receiver content = Out_channel.output_string oc content in
  p receiver;
  Out_channel.close oc

let ref_to_path p h =
  let folder_name_len = 2 in
  let folder_cnt = 4 in
  let hash_len = 32 in
  let rec add_path_sep p s =
    let slen = String.length s in
    if slen <= hash_len - (folder_cnt * folder_name_len) then p ^ s
    else
      let f = String.sub s 0 folder_name_len in
      let r = String.sub s folder_name_len (slen - folder_name_len) in
      add_path_sep (p ^ f ^ "/") r
  in
  let hash_path = add_path_sep "" h in
  Filename.concat p hash_path

module type StorageType = sig
  type t
  type config

  val create : config -> t
  val load_index_config : t -> Config.IndexConfig.t
  val save_index_config : Config.IndexConfig.t -> t -> unit
  val index_config_exists : t -> bool
  val load_doc_table : DocumentTable.Id.t -> t -> DocumentTable.t
  val save_doc_table : DocumentTable.t -> t -> unit
  val load_token_table : t -> TokenTable.t
  val save_token_table : TokenTable.t -> t -> unit
  val load_doc : Document.Id.t -> t -> Document.t
  val load_doc_opt : Document.Id.t -> t -> Document.t option
  val save_doc : Document.t -> t -> unit
  val lock : ?force:bool -> t -> unit
  val unlock : t -> unit
  val with_lock : ?force:bool -> (unit -> 'a) -> t -> 'a
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

let file_exists = Sys_unix.file_exists_exn ~follow_symlinks:true

module FileStorage = struct
  type config = string
  type t = { base_path : string }

  let create path = { base_path = path }

  module Doc_table_file = struct
    let position_list_to_string e =
      e |> List.map string_of_int |> String.concat " " |> String.trim

    let id_to_path id =
      ref_to_path (DocumentTable.Id.prefix id) (DocumentTable.Id.hash id)

    let parse_row s =
      let rl =
        String.trim s |> String.split_on_char ' '
        |> List.filter (fun t -> String.length t > 0)
      in
      match rl with
      | [ _ ] | [] -> None
      | ref :: flags :: pl -> Some (ref, flags, List.map int_of_string pl)

    let load k conf =
      let dt = DocumentTable.empty k in
      let filename = Filename.concat conf.base_path (id_to_path k) in
      if Sys.file_exists filename then
        let rec add_rows t rl =
          match rl with
          | [] -> t
          | r :: rest ->
              let tu =
                match parse_row r with
                | Some (r, flags, pl) ->
                    DocumentTable.add (Document.Id.of_string r) (Text.TokenEntry.Flags.from_string flags, pl) t
                | None -> t
              in
              add_rows tu rest
        in
        add_rows dt (String.split_on_char '\n' (read_file filename))
      else dt

    let save ti conf =
      let filename =
        Filename.concat conf.base_path (id_to_path (DocumentTable.id ti))
      in
      let producer receiver =
        let rec loop = function
          | [] -> ()
          | (r, (flags, pl)) :: rest ->
              receiver
                (Document.Id.to_string r ^ " " ^ (Text.TokenEntry.Flags.to_string flags) ^ " " ^ position_list_to_string pl
               ^ "\n");
              loop rest
        in
        loop (Document.DocumentMap.to_list ti.map)
      in
      write_file_with_producer producer filename
  end

  module Token_table_file = struct
    let filename conf = Filename.concat conf.base_path "token-table"

    let load conf =
      let tt = TokenTable.empty in
      let f = filename conf in
      if Sys.file_exists f then
        let rec add_rows t = function
          | [] -> t
          | r :: rest ->
              let tr =
                match String.split_on_char ' ' r with
                | [ term; dtref ] ->
                    TokenTable.add term (DocumentTable.Id.of_string dtref) t
                | _ -> t
              in
              add_rows tr rest
        in
        add_rows tt (String.split_on_char '\n' (read_file f))
      else tt

    let save tt conf =
      let f = filename conf in
      let list =
        TokenMap.to_list tt
        |> List.map (fun (term, dtref) ->
               term ^ " " ^ DocumentTable.Id.to_string dtref)
      in
      let producer receiver =
        let rec loop = function
          | [] -> ()
          | line :: r ->
              receiver (line ^ "\n");
              loop r
        in
        loop list
      in
      write_file_with_producer producer f
  end

  module Doc_file = struct
    let id_to_path id =
      ref_to_path (Document.Id.prefix id) (Document.Id.hash id)

    let filenames id conf =
      let path = Filename.concat conf.base_path (id_to_path id) in
      (Filename.concat path "meta", Filename.concat path "content")

    let load id conf =
      let meta_file, content_file = filenames id conf in
      let meta =
        Document.Meta.t_of_sexp (Core.Sexp.of_string (read_file meta_file))
      in
      let content = read_file content_file in
      Document.create meta content

    let load_opt id conf =
      let meta_file, content_file = filenames id conf in
      if Sys.file_exists meta_file && Sys.file_exists content_file then
        Some (load id conf)
      else None

    let save d conf =
      let meta_file, content_file = filenames (Document.id d) conf in
      write_file
        (Core.Sexp.to_string (Document.Meta.sexp_of_t (Document.meta d)))
        meta_file;
      write_file (Document.content d) content_file
  end

  let config_file_path conf = Filename.concat conf.base_path "config"
  let lock_file_path conf = Filename.concat conf.base_path "lock"
  let load_doc_table = Doc_table_file.load
  let save_doc_table = Doc_table_file.save
  let load_token_table = Token_table_file.load
  let save_token_table = Token_table_file.save
  let load_doc = Doc_file.load
  let load_doc_opt = Doc_file.load_opt
  let save_doc = Doc_file.save

  let load_index_config conf =
    Config.IndexConfig.t_of_sexp
      (Core.Sexp.of_string (read_file (config_file_path conf)))

  let save_index_config ic conf =
    let cpath = config_file_path conf in
    Logs.debug (fun m -> m "Write index config to: %s" cpath);
    write_file (Core.Sexp.to_string (Config.IndexConfig.sexp_of_t ic)) cpath

  let index_config_exists conf = file_exists (config_file_path conf)

  let lock ?(force = false) conf =
    let perm = [ Unix.O_CREAT; Unix.O_WRONLY ] in
    let perm = if force then perm else perm @ [ Unix.O_EXCL ] in
    let path = lock_file_path conf in
    create_dirs path;
    let fd = Unix.openfile path perm 0o600 in
    Unix.close fd;
    Logs.info (fun m -> m "Index locked")

  let unlock conf =
    Unix.unlink (lock_file_path conf);
    Logs.info (fun m -> m "Index unlocked")

  let with_lock ?(force = false) f conf =
    let finally () = unlock conf in
    let work () =
      lock ~force conf;
      f ()
    in
    Fun.protect ~finally work
end

let file_storage path = storage (module FileStorage) path
let is_directory = Sys_unix.is_directory_exn ~follow_symlinks:false

let find_all_files ~extension dir =
  let rec loop result = function
    (* by not following the symlinks we handle symlinks pointing to a dir as file *)
    | f :: tl when is_directory f ->
        Sys_unix.ls_dir f
        |> List.map (Filename.concat f)
        |> List.filter file_exists |> List.append tl |> loop result
    | f :: tl when Filename.extension f = "." ^ extension ->
        loop (f :: result) tl
    | _ :: tl -> loop result tl
    | [] -> result
  in
  (* after we finished the loop we have to filter out all symbolic links pointing to directories *)
  loop [] (dir :: [])
  |> List.filter (fun f ->
         not (Sys_unix.is_directory_exn ~follow_symlinks:true f))
