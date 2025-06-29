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
  if not (Sys.file_exists dir && Sys.is_directory dir) then (
    create_dirs dir;
    Sys.mkdir dir 0o755)

let write_file content filename =
  create_dirs filename;
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc content;
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
  val load_doc_table : Model.DocumentTable.Id.t -> t -> Model.DocumentTable.t
  val save_doc_table : Model.DocumentTable.t -> t -> unit
  val load_token_table : t -> Model.TokenTable.t
  val save_token_table : Model.TokenTable.t -> t -> unit
  val load_doc : Model.Document.Id.t -> t -> Model.Document.t
  val save_doc : Model.Document.t -> t -> unit
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

module FileStorage = struct
  type config = string
  type t = { base_path : string }

  let create path = { base_path = path }

  module Doc_table_file = struct
    let position_list_to_string e =
      e |> List.map string_of_int |> String.concat " " |> String.trim

    let id_to_path id =
      ref_to_path
        (Model.DocumentTable.Id.prefix id)
        (Model.DocumentTable.Id.hash id)

    let entry_to_string ti =
      let open Model.DocumentTable in
      let rec build el s =
        match el with
        | [] -> s
        | (r, e) :: rest ->
            build rest
              (s
              ^ Model.Document.Id.to_string r
              ^ " " ^ position_list_to_string e ^ "\n")
      in
      build (Model.DocumentMap.to_list ti.map) ""

    let parse_row s =
      let rl =
        String.trim s |> String.split_on_char ' '
        |> List.filter (fun t -> String.length t > 0)
      in
      match rl with
      | [ _ ] | [] -> None
      | ref :: pl -> Some (ref, List.map int_of_string pl)

    let load k conf =
      let dt = Model.DocumentTable.empty k in
      let filename = Filename.concat conf.base_path (id_to_path k) in
      if Sys.file_exists filename then
        let rec add_rows t rl =
          match rl with
          | [] -> t
          | r :: rest ->
              let tu =
                match parse_row r with
                | Some (r, pl) ->
                    Model.DocumentTable.add (Model.Document.Id.of_string r) pl t
                | None -> t
              in
              add_rows tu rest
        in
        add_rows dt (String.split_on_char '\n' (read_file filename))
      else dt

    let save ti conf =
      let filename =
        Filename.concat conf.base_path (id_to_path (Model.DocumentTable.id ti))
      in
      write_file (entry_to_string ti) filename
  end

  module Token_table_file = struct
    let filename conf = Filename.concat conf.base_path "term-table"

    let load conf =
      let tt = Model.TokenTable.empty in
      let f = filename conf in
      if Sys.file_exists f then
        let rec add_rows t = function
          | [] -> t
          | r :: rest ->
              let tr =
                match String.split_on_char ' ' r with
                | [ term; dtref ] ->
                    Model.TokenTable.add term
                      (Model.DocumentTable.Id.of_string dtref)
                      t
                | _ -> t
              in
              add_rows tr rest
        in
        add_rows tt (String.split_on_char '\n' (read_file f))
      else tt

    let save tt conf =
      let rec entry_to_string s = function
        | [] -> s
        | (term, dtref) :: rest ->
            entry_to_string
              (s ^ term ^ " " ^ Model.DocumentTable.Id.to_string dtref ^ "\n")
              rest
      in
      let cnt = entry_to_string "" (Model.TokenMap.to_list tt) in
      let f = filename conf in
      write_file cnt f
  end

  module Doc_file = struct
    let id_to_path id =
      ref_to_path (Model.Document.Id.prefix id) (Model.Document.Id.hash id)

    let filenames id conf =
      let path = Filename.concat conf.base_path (id_to_path id) in
      (Filename.concat path "meta", Filename.concat path "content")

    let load id conf =
      let meta_file, content_file = filenames id conf in
      let meta =
        Model.Document.Meta.t_of_sexp
          (Core.Sexp.of_string (read_file meta_file))
      in
      let content = read_file content_file in
      Model.Document.create meta content

    let save d conf =
      let meta_file, content_file = filenames (Model.Document.id d) conf in
      write_file
        (Core.Sexp.to_string
           (Model.Document.Meta.sexp_of_t (Model.Document.meta d)))
        meta_file;
      write_file (Model.Document.content d) content_file
  end

  let lock_file_path conf = Filename.concat conf.base_path "lock"
  let load_doc_table = Doc_table_file.load
  let save_doc_table = Doc_table_file.save
  let load_token_table = Token_table_file.load
  let save_token_table = Token_table_file.save
  let load_doc = Doc_file.load
  let save_doc = Doc_file.save

  let lock ?(force = false) conf =
    let perm = [ Unix.O_CREAT; Unix.O_WRONLY ] in
    let perm = if force then perm else perm @ [ Unix.O_EXCL ] in
    let path = lock_file_path conf in
    create_dirs path;
    let fd = Unix.openfile path perm 0o600 in
    Unix.close fd

  let unlock conf = Unix.unlink (lock_file_path conf)

  let with_lock ?(force = false) f conf =
    let finally () = unlock conf in
    let work () =
      lock ~force conf;
      f ()
    in
    Fun.protect ~finally work
end

let file_storage path = storage (module FileStorage) path
