open Hit

let check_config index_path =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  if not (S.Impl.index_config_exists S.t) then (
    print_endline ("ERROR: cannot find index data structure in " ^ index_path);
    ignore (exit 1))

let init_logging l =
  let level =
    match l with
    | "error" -> Some Logs.Error
    | "warn" -> Some Logs.Warning
    | "info" -> Some Logs.Info
    | "debug" -> Some Logs.Debug
    | _ ->
        print_endline ("unknown log level: " ^ l);
        None
  in
  Logs.set_level level

let pp_time ppf () =
  let tm = Unix.localtime (Unix.time ()) in
  Format.fprintf ppf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

let pp_header ppf (level, _) =
  Format.fprintf ppf "%a %a " pp_time () Logs_fmt.pp_header (level, None)

let read_document document_source document_path =
  let open Document in
  from_source document_source document_path (Io.read_file document_path)

let doc_representation d =
  let open Document in
  Id.to_string (id d) ^ " " ^ Meta.path (meta d)

let add_document ?(force = false) index_path document_path document_source =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.load () in
  Logs.info (fun m -> m "Add document: %s" document_path);
  let d = read_document document_source document_path in
  let idx' = Idx.add_doc d idx in
  ignore (Idx.flush ~force idx');
  d

let delete_document ?(force = false) index_path document_id =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.load () in
  Logs.info (fun m -> m "Delete document: %s" document_id);
  let idx' = Idx.delete_doc (Document.Id.of_string document_id) idx in
  ignore (Idx.flush ~force idx')

let import_documents ~extension ?(force = false) index_path directory
    document_source =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.load () in
  Logs.info (fun m -> m "Import documents: type=%s path=%s" extension directory);
  let idx', dl =
    let dot_extension = "." ^ extension in
    Io.find_all_files
      ~predicate:(fun f -> Filename.extension f = dot_extension)
      directory
    |> List.map (read_document document_source)
    |> List.fold_left (fun (idx, l) d -> (Idx.add_doc d idx, d :: l)) (idx, [])
  in
  ignore (Idx.flush ~force idx');
  dl

let init index_path =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  ignore (Idx.init ())

let to_result_list get_doc count docs =
  let rl =
    docs |> List.map (fun sr -> (get_doc (Index.QueryResult.doc_id sr), sr))
  in
  match count with c when c < 1 -> rl | c -> List.take c rl

let search index_path count words =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let module Q = Index.Query.Make (Idx) in
  let idx = Idx.load () in
  let terms = List.map String.lowercase_ascii words in
  let docs = Q.find_docs terms idx in
  to_result_list Idx.get_doc count docs

let query index_path count q =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.load () in
  let module Q = Index.Query.Make (Idx) in
  let docs = Q.query (Index.Query.from_string q) idx in
  to_result_list Idx.get_doc count docs

type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let parse_color s =
  match String.lowercase_ascii s with
  | "black" -> Some Black
  | "red" -> Some Red
  | "green" -> Some Green
  | "yellow" -> Some Yellow
  | "blue" -> Some Blue
  | "magenta" -> Some Magenta
  | "cyan" -> Some Cyan
  | "white" -> Some White
  | _ -> None

let color_to_code c =
  match c with
  | Black -> 30
  | Red -> 31
  | Green -> 32
  | Yellow -> 33
  | Blue -> 34
  | Magenta -> 35
  | Cyan -> 36
  | White -> 37

let preview_to_string p color =
  let remove_linefeed s = Str.global_replace (Str.regexp "[\n|\r]+") " " s in
  let open View.Preview in
  let to_string e =
    match e with
    | Text s -> remove_linefeed s
    | Token s -> (
        match color with
        | Some c ->
            "\027["
            ^ string_of_int (color_to_code c)
            ^ "m\027[1m" ^ remove_linefeed s ^ "\027[0m"
        | None -> remove_linefeed s)
  in
  List.map to_string p |> String.concat ""

let base_path_flag =
  let open Command.Param in
  flag "-d"
    (optional_with_default "." string)
    ~doc:" base directory of the index"

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
  flag "-l"
    (optional_with_default "warn" string)
    ~doc:" set logging level (default warn)"

let setup_command =
  Command.basic ~summary:"initialize the index data directory"
    Command.Let_syntax.(
      let%map_open base_path = base_path_flag and log = log_flag in
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
        let d = add_document ~force base_path document source in
        print_endline (doc_representation d);
        ())

let delete_command =
  Command.basic ~summary:"delete a document from the index"
    Command.Let_syntax.(
      let%map_open document_id = anon ("document_id" %: string)
      and base_path = base_path_flag
      and force = force_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        delete_document ~force base_path document_id)

let gc_command =
  Command.basic ~summary:"garbage collect the index"
    Command.Let_syntax.(
      let%map_open base_path = base_path_flag
      and force = force_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        let module S = (val Io.file_storage base_path : Io.StorageInstance) in
        let module Idx = Index.Make (S) in
        let idx = Idx.load () in
        let idx' = Idx.garbage_collect idx in
        ignore (Idx.flush ~force idx'))

let import_command =
  Command.basic
    ~summary:"import a documents of the given directory to the index"
    Command.Let_syntax.(
      let%map_open dir = anon ("directory" %: string)
      and base_path = base_path_flag
      and source = source_flag
      and extension = flag "-t" (required string) ~doc:" file type to import"
      and force = force_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        let dl = import_documents ~extension ~force base_path dir source in
        List.iter (fun d -> print_endline (doc_representation d)) dl)

let search_command =
  Command.basic ~summary:"search for a term in the index"
    Command.Let_syntax.(
      let%map_open terms = anon (sequence ("terms" %: string))
      and details = flag "-m" no_arg ~doc:" show matches"
      and color =
        flag "-C"
          (optional_with_default "yellow" string)
          ~doc:
            " highlight matches with the given color (black, red, green, \
             yellow, blue, magenta, cyan, white)"
      and count =
        flag "-c"
          (optional_with_default 0 int)
          ~doc:" max count of documents returned (default 0 - no limit)"
      and base_path = base_path_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        let docs = search base_path count terms in
        List.iter
          (fun (doc, sr) ->
            let p =
              if details then
                ": "
                ^ preview_to_string
                    (View.Preview.create doc sr |> View.Preview.shorten)
                    (parse_color color)
              else ""
            in
            print_endline (doc_representation doc ^ p))
          docs)

let query_command =
  Command.basic ~summary:"query the index with the hit query language HQL"
    Command.Let_syntax.(
      let%map_open q = anon ("query" %: string)
      and details = flag "-m" no_arg ~doc:" show matches"
      and color =
        flag "-C"
          (optional_with_default "yellow" string)
          ~doc:
            " highlight matches with the given color (black, red, green, \
             yellow, blue, magenta, cyan, white)"
      and count =
        flag "-c"
          (optional_with_default 0 int)
          ~doc:" max count of documents returned (default 0 - no limit)"
      and base_path = base_path_flag
      and log = log_flag in
      fun () ->
        check_config base_path;
        init_logging log;
        let docs = query base_path count q in
        List.iter
          (fun (doc, sr) ->
            let p =
              if details then
                ": "
                ^ preview_to_string
                    (View.Preview.create doc sr |> View.Preview.shorten)
                    (parse_color color)
              else ""
            in
            print_endline (doc_representation doc ^ p))
          docs)

let main_command =
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  Command.group ~summary:"hit commands"
    [
      ("init", setup_command);
      ("add", add_command);
      ("search", search_command);
      ("query", query_command);
      ("import", import_command);
      ("delete", delete_command);
      ("gc", gc_command);
    ]

let () =
  Command_unix.run main_command;
  Logs.info (fun m -> m "Done")
