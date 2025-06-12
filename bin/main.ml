let add_command =
  Command.basic ~summary:"add a document to the index"
    Command.Let_syntax.(
      let%map_open document = anon ("document" %: string) in
      fun () -> print_endline document)

let search_command =
  Command.basic ~summary:"search for a term in the index"
    Command.Let_syntax.(
      let%map_open term = anon ("term" %: string) in
      fun () -> print_endline term)

let main_command =
  Command.group ~summary:"hit commands"
    [ ("add", add_command); ("search", search_command) ]

let () = Command_unix.run main_command
