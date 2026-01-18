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
  and doc1 =
    Document.create (Document.Meta.create "local" "/path/to/doc1.txt" "") ""
  and doc2 =
    Document.create
      (Document.Meta.create "local" "/path/to/doc2.txt" "")
      "(1) (4) hrg ks dso (hxx) (22) (gg (89)) [33] fhjd d www s a   saa s sa \
       sa sa sa de r  dhfhg dsmue "
  and doc3 =
    Document.create
      (Document.Meta.create "local" "/path/to/doc2.txt" "")
      "(1) (4) hrg ks dso (hxx) (22) (gg (89)) [33] fhjd d www s a 777777 \
       yrgsv xxx222  saa s sa sa sa sa de r  \n\
       dhfhg dsmue gg foo bar 563 gggx fffffxx htx hml html xml css qqq aaa \
       222222 vvv htygr hdfgddds tom"
  and doc4 =
    Document.create
      (Document.Meta.create "local" "/path/to/doc2.txt" "")
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
       tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
       veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
       commodo consequat. Duis aute irure dolor in reprehenderit in voluptate \
       velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
       occaecat cupidatat non proident, sunt in culpa ..."
  and doc5 =
    Document.create
      (Document.Meta.create "local" "/path/to/doc2.txt" "")
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
       tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
       veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
       commodo consequat. Duis aute irure dolor in reprehenderit in voluptate \
       velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
       occaecat cupidatat non proident, sunt in culpa qui officia deserunt \
       mollit anim id est laborum. 10 11 12 13 14 15 16 17 18 19 20 21\n"
  in
  [
    (fun () -> ignore (Text.Parser.parse separators doc0))
    |> Bench.Test.create ~name:"Parser: parse doc0 (empty)";
    (fun () -> ignore (Text.Parser.parse separators doc1))
    |> Bench.Test.create ~name:"Parser: parse doc1 (only path)";
    (fun () -> ignore (Text.Parser.parse separators doc2))
    |> Bench.Test.create ~name:"Parser: parse doc2 (20 tokens)";
    (fun () -> ignore (Text.Parser.parse separators doc3))
    |> Bench.Test.create ~name:"Parser: parse doc3 (40 tokens)";
    (fun () -> ignore (Text.Parser.parse separators doc4))
    |> Bench.Test.create ~name:"Parser: parse doc4 (60 tokens)";
    (fun () -> ignore (Text.Parser.parse separators doc5))
    |> Bench.Test.create ~name:"Parser: parse doc5 (80 tokens)";
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
       @ parser_benchmark))
