open Core_bench
open Hit

let benchmark index_path =
  if Sys.file_exists index_path then (
    print_endline ("ERROR: Directory already exists: " ^ index_path);
    exit 1);
  Sys.mkdir index_path 0o755;
  let module FileStore = (val Io.file_storage index_path : Io.StorageInstance)
  in
  let module FileIdx = Index.Make (FileStore) in
  let module FileIdxQuery = Index.Query.Make (FileIdx) in
  let init_index (module Idx : Index.IndexType) =
    ignore (FileIdx.create ());
    let idx = Idx.load () in
    let idx = Idx.clear idx in
    let idx =
      idx
      |> Idx.add_doc
           (Document.from_source "local" "/documents/doc1.txt"
              "This is the content of document one.")
      |> Idx.add_doc
           (Document.from_source "local" "/documents/doc2.txt"
              "This is the content of document two.")
      |> Idx.add_doc
           (Document.from_source "local" "/documents/doc3.txt"
              "This is the content of document three.")
      |> Idx.add_doc
           (Document.from_source "local" "/documents/doc4.txt"
              "This is the very long content\n\
              \                                                       Lorem \
               ipsum dolor sit amet, consetetur sadipscing elitr,\n\
              \                                                       sed diam \
               nonumy eirmod tempor invidunt ut labore ets\n\
              \                                                       dolore \
               magna aliquyam erat, sed diam voluptua\n\
              \                                                       of \
               document four.")
    in
    Idx.flush idx
  in
  let with_file_index f =
   fun `init ->
    let idx = init_index (module FileIdx) in
    fun () -> f idx
  in
  [
    (fun idx -> FileIdxQuery.find_docs [ "document"; "one" ] idx)
    |> with_file_index
    |> Bench.Test.create_with_initialization ~name:"Query.find_docs";
    (fun idx ->
      FileIdx.add_doc
        (Document.from_source "local" "/documents/doc1.txt"
           "This is the content of document one.")
        idx)
    |> with_file_index
    |> Bench.Test.create_with_initialization ~name:"Index.add_doc (existing)";
  ]

let () =
  let index_path =
    "hit_benchmark_index_" ^ string_of_float (Unix.gettimeofday ())
  in
  Command_unix.run (Bench.make_command (benchmark index_path))
