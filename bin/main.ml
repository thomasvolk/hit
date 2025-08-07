open Hit

let check_config index_path =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  if not (S.Impl.index_config_exists S.t) then (
    print_endline ("ERROR: cannot find index data structure in " ^ index_path);
    ignore (exit 1))

let init_logging info =
  let level = if info then Some Logs.Info else Some Logs.Warning in
  Logs.set_level level

let pp_time ppf () =
  let tm = Unix.localtime (Unix.time ()) in
  Format.fprintf ppf "%02d:%02d:%02d"
  tm.tm_hour tm.tm_min tm.tm_sec

let pp_header ppf (level, _) =
  Format.fprintf ppf "%a %a " pp_time () Logs_fmt.pp_header (level, None)

let read_document document_source document_path =
  let open Table.Document in
  create
    (Meta.create document_source document_path)
    (Io.read_file document_path)

let add_document ?(force = false) index_path document_path document_source =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.load () in
  let d = read_document document_source document_path in
  let idx' = Idx.add_doc d idx in
  Idx.flush ~force idx'

let import_documents ~extension ?(force = false) index_path directory
    document_source =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.load () in
  let idx' =
    Io.find_all_files ~extension directory
    |> List.map (read_document document_source)
    |> List.fold_left (fun idx d -> Idx.add_doc d idx) idx
  in
  Idx.flush ~force idx'

let init index_path =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  Idx.init ()

let search index_path words =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.load () in
  let terms = List.map String.lowercase_ascii words in
  Idx.find_docs terms idx
  |> List.map (fun sr ->
         ( Idx.get_doc (Index.SearchResult.doc_id sr),
           Index.SearchResult.token_entries sr ))

let print_highlight h =
  let open View.Highlight in
  let line_to_string l =
    let n = Line.number l in
    let parts =
      Line.parts l
      |> List.map (fun p ->
             match p with Text s -> s | Token s -> "\027[1m" ^ s ^ "\027[0m")
    in
    let num =
      String.make (6 - String.length (string_of_int n)) ' ' ^ string_of_int n
    in
    num ^ ": " ^ " " ^ String.concat "" parts
  in
  List.map line_to_string h |> List.iter print_endline

let base_path_flag =
  let open Command.Param in
  flag "-d" (optional_with_default "." string) ~doc:" base directory of the index"

let source_flag =
  let open Command.Param in
  flag "-s"
    (optional_with_default "local" string)
    ~doc:" document source (default local)"

let force_flag =
  let open Command.Param in
  flag "-f" no_arg ~doc:" force writing to the index by ignoring the lock"

let log_flag =
  let open Command.Param in
  flag "-l" no_arg ~doc:" enable logging"

let setup_command =
  Command.basic ~summary:"initialize the index data directory"
    Command.Let_syntax.(
      let%map_open base_path = base_path_flag
      and log = log_flag in
      fun () ->
        init_logging log;
        init base_path)

let add_command =
  Command.basic ~summary:"add a document to the index"
    Command.Let_syntax.(
      let%map_open document = anon ("document" %: string)
      and base_path = base_path_flag
      and source = source_flag
      and force = force_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        let _i = add_document ~force base_path document source in
        ())

let import_command =
  Command.basic
    ~summary:"import a documents of the given directory to the index"
    Command.Let_syntax.(
      let%map_open dir = anon ("directory" %: string)
      and base_path = base_path_flag
      and source = source_flag
      and extension =
        flag "-t" (required string) ~doc:" file type to import"
      and force = force_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        let _i = import_documents ~extension ~force base_path dir source in
        ())

let search_command =
  Command.basic ~summary:"search for a term in the index"
    Command.Let_syntax.(
      let%map_open terms = anon (sequence ("terms" %: string))
      and details = flag "-m" no_arg ~doc:" show matches"
      and base_path = base_path_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        let docs = search base_path terms in
        let open Table.Document in
        List.iter
          (fun (doc, tl) ->
            print_endline (Id.to_string (id doc) ^ " - " ^ Meta.id (meta doc));
            if details then print_highlight (View.Highlight.create doc tl))
          docs)

let main_command =
  Logs.set_reporter (Logs_fmt.reporter ~pp_header:pp_header ());
  Command.group ~summary:"hit commands"
    [
      ("init", setup_command);
      ("add", add_command);
      ("search", search_command);
      ("import", import_command);
    ]

let () = Command_unix.run main_command
