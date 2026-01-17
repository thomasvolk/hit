open Core_bench
open Hit

let index_benchmark name (module Idx : Index.IndexType)
    (module Query : Index.Query.QueryType) =
  let init_index =
    ignore (Idx.create ());
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
  let with_index f =
   fun `init ->
    let idx = init_index in
    fun () -> f idx
  in
  [
    with_index (fun idx -> Query.find_docs [ "document"; "one" ] idx)
    |> Bench.Test.create_with_initialization ~name:(name ^ ": Query.find_docs");
    with_index (fun idx ->
        Idx.add_doc
          (Document.from_source "local" "/documents/doc1.txt"
             "This is the content of document one.")
          idx)
    |> Bench.Test.create_with_initialization
         ~name:(name ^ ": Index.add_doc (existing)");
  ]

let parser_benchmark =
let separators = String.to_seq Config.default_separators |> List.of_seq
and doc0 = Document.create (Document.Meta.create "" "" "") ""
and doc1 = Document.create (Document.Meta.create "local" "/path/to/doc1.txt" "") ""
and doc2 = Document.create (Document.Meta.create "local" "/path/to/doc2.txt" "")
"(1) (4) hrg ks dso (hxx) (22) (gg (89)) [33] fhjd d www s a   saa s sa sa sa sa de r  dhfhg dsmue "
in
  [
    (fun () -> ignore (Text.Parser.parse separators doc0)) |> Bench.Test.create ~name:("Parser: parse doc0 (empty)");
    (fun () -> ignore (Text.Parser.parse separators doc1)) |> Bench.Test.create ~name:("Parser: parse doc1 (only path)");
    (fun () -> ignore (Text.Parser.parse separators doc2)) |> Bench.Test.create ~name:("Parser: parse doc2");
  ]

let () =
  let index_path =
    "hit_benchmark_index_" ^ string_of_float (Unix.gettimeofday ())
  in
  if Sys.file_exists index_path then (
    print_endline ("ERROR: Directory already exists: " ^ index_path);
    exit 1);
  Sys.mkdir index_path 0o755;
  let module FileStore = (val Io.file_storage index_path : Io.StorageInstance)
  in
  let module FileIdx = Index.Make (FileStore) in
  let module FileIdxQuery = Index.Query.Make (FileIdx) in
  let module MemoryStore =
    (val Io.in_memory_storage (100, 100) : Io.StorageInstance)
  in
  let module MemoryIdx = Index.Make (MemoryStore) in
  let module MemoryIdxQuery = Index.Query.Make (MemoryIdx) in
  Command_unix.run
    (Bench.make_command
       (index_benchmark "Filesystem" (module FileIdx) (module FileIdxQuery)
       @ index_benchmark "Memory" (module MemoryIdx) (module MemoryIdxQuery)
       @ parser_benchmark ))
