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


let main_command =
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  Command.group ~summary:"hit commands"
    [
    ]

let () =
  Command_unix.run main_command;
  Logs.info (fun m -> m "Done")
