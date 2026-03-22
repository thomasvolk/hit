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

let log_flag =
  let open Command.Param in
  flag "-l"
    (optional_with_default "warn" string)
    ~doc:" set logging level (default warn)"

let base_path_flag =
  let open Command.Param in
  flag "-d"
    (optional_with_default "." string)
    ~doc:" base directory of the index"

let delete_command =
  Command.basic ~summary:"delete a file from the index"
    Command.Let_syntax.(
      let%map_open _document_id = anon ("document_id" %: string)
      and _base_path = base_path_flag
      and log = log_flag in
      fun () ->
        init_logging log;
        print_endline "delete - not implemented")

let query_command =
  Command.basic ~summary:"query the index"
    Command.Let_syntax.(
      let%map_open _query = anon ("query" %: string)
      and _base_path = base_path_flag
      and log = log_flag in
      fun () ->
        init_logging log;
        print_endline "query - not implemented")

let add_command =
  Command.basic ~summary:"add a file to the index"
    Command.Let_syntax.(
      let%map_open _document = anon ("document" %: string)
      and _base_path = base_path_flag
      and log = log_flag in
      fun () ->
        init_logging log;
        print_endline "add - not implemented")

let dump_command =
  Command.basic ~summary:"dump the index in s-expression format"
    Command.Let_syntax.(
      let%map_open _base_path = base_path_flag and log = log_flag in
      fun () ->
        init_logging log;
        print_endline "dump - not implemented")

let main_command =
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  Command.group ~summary:"hit commands"
    [
      ("add", add_command);
      ("query", query_command);
      ("delete", delete_command);
      ("dump", dump_command);
    ]

let () =
  Command_unix.run main_command;
  Logs.info (fun m -> m "Done")
