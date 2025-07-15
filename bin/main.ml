open Hit

let read_document document_source document_path =
  let open Table.Document in
  create
    (Meta.create document_source document_path)
    (Io.read_file document_path)

let add_document ?(force = false) index_path document_path document_source =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.create in
  let d = read_document document_source document_path in
  let idx' = Idx.add_doc d idx in
  Idx.flush ~force idx'

let import_documents ~extension ?(force = false) index_path directory
    document_source =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.create in
  let idx' =
    Io.find_all_files ~extension directory
    |> List.map (read_document document_source)
    |> List.fold_left (fun idx d -> Idx.add_doc d idx) idx
  in
  Idx.flush ~force idx'

let search index_path words =
  let module S = (val Io.file_storage index_path : Io.StorageInstance) in
  let module Idx = Index.Make (S) in
  let idx = Idx.create in
  let terms = List.map String.lowercase_ascii words in
  Idx.find_docs terms idx |> List.map (fun (did, tl) -> (Idx.get_doc did, tl))

let print_highlight h =
  let open View.Highlight in
  let line_to_string l =
    let n = Line.number l in
    let parts = Line.parts l
      |> List.map (fun p -> match p with | Text(t) -> t | _ -> "")
    in
    (string_of_int n) ^ " " ^ " " ^ (String.concat "" parts)
  in
  List.map line_to_string h |> List.iter print_endline

let base_path_flag =
  let open Command.Param in
  flag "-p" (optional_with_default "." string) ~doc:" base path of the index"

let source_flag =
  let open Command.Param in
  flag "-s"
    (optional_with_default "local" string)
    ~doc:" document source (default local)"

let force_flag =
  let open Command.Param in
  flag "-f" no_arg ~doc:"force writing to the index by ignoring the lock"

let add_command =
  Command.basic ~summary:"add a document to the index"
    Command.Let_syntax.(
      let%map_open document = anon ("document" %: string)
      and base_path = base_path_flag
      and source = source_flag
      and force = force_flag in
      fun () ->
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
        flag "-e" (required string) ~doc:" extension of file to import"
      and force = force_flag in
      fun () ->
        let _i = import_documents ~extension ~force base_path dir source in
        ())

let search_command =
  Command.basic ~summary:"search for a term in the index"
    Command.Let_syntax.(
      let%map_open terms = anon (sequence ("terms" %: string))
      and details =
        flag "-d" no_arg ~doc:" show details"
      and base_path = base_path_flag in
      fun () ->
        let docs = search base_path terms in
        let open Table.Document in
        List.iter
          (fun (doc, tl) ->
            print_endline (Id.to_string (id doc) ^ " - " ^ Meta.id (meta doc));
            if details then print_highlight (View.Highlight.create doc tl)
            )
          docs)

let main_command =
  Command.group ~summary:"hit commands"
    [
      ("add", add_command);
      ("search", search_command);
      ("import", import_command);
    ]

let () = Command_unix.run main_command
