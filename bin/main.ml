open Hit

let add_document index_path document_path document_source =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.create in
  let open Model.Document in
  let d =
    create
      (Meta.create document_source document_path)
      (Io.read_file document_path)
  in
  let idx' = Idx.add_doc d idx in
  Idx.flush idx'

let search index_path term =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.create in
  Idx.find_docs term idx |> List.map Idx.get_doc

let base_path_flag =
  let open Command.Param in
  flag "-p" (optional_with_default "." string) ~doc:" base path of the index"

let source_flag =
  let open Command.Param in
  flag "-s"
    (optional_with_default "local" string)
    ~doc:" document source (default local)"

let add_command =
  Command.basic ~summary:"add a document to the index"
    Command.Let_syntax.(
      let%map_open document = anon ("document" %: string)
      and base_path = base_path_flag
      and source = source_flag in
      fun () ->
        let _i = add_document base_path document source in
        ())

let search_command =
  Command.basic ~summary:"search for a term in the index"
    Command.Let_syntax.(
      let%map_open terms = anon (sequence ("terms" %: string))
      and base_path = base_path_flag in
      fun () ->
        let docs = search base_path terms in
        let open Model.Document in
        List.iter
          (fun doc ->
            print_endline (Id.to_string (id doc) ^ " - " ^ Meta.id (meta doc)))
          docs)

let main_command =
  Command.group ~summary:"hit commands"
    [ ("add", add_command); ("search", search_command) ]

let () = Command_unix.run main_command
