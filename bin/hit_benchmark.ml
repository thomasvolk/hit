open Core_bench
open Hit

let index_benchmark path =
  let init_index =
    let idx = Index.create path in
    ignore (Index.add idx
         "/documents/doc1.txt"
            "This is the content of document one.");
    ignore (Index.add idx
         "/documents/doc2.txt"
            "This is the content of document two.");
    ignore (Index.add idx
         "/documents/doc3.txt"
            "This is the content of document three.");
    ignore (Index.add idx
         "/documents/doc4.txt"
            "This is the very long content\n\
            \                                                       Lorem \
             ipsum dolor sit amet, consetetur sadipscing elitr,\n\
            \                                                       sed diam \
             nonumy eirmod tempor invidunt ut labore ets\n\
            \                                                       dolore \
             magna aliquyam erat, sed diam voluptua\n\
            \                                                       of \
             document four.");
    idx
  in
  let with_index f =
   fun `init ->
    let idx = init_index in
    fun () -> f idx
  in
  let count = ref 0 in
  [
    with_index (fun idx -> Index.query idx "(eq two)")
    |> Bench.Test.create_with_initialization ~name:("Index.query");
    with_index (fun idx ->
        ignore
          (Index.add idx "/documents/doc1.txt"
                "This is the content of document one."))
    |> Bench.Test.create_with_initialization
         ~name:("Index.add (existing)");
    with_index (fun idx ->
        ignore
          (Index.add idx (Printf.sprintf "/documents/doc%d.txt" !count)
          (Printf.sprintf "This is the content of document %d." !count));
           count := !count + 1)
    |> Bench.Test.create_with_initialization
         ~name:("Index.add (new)");
  ]

let parser_benchmark =
  let doc0 = ""
  and doc1 = "/path/to/doc1.txt"
  and doc2 = "/path/to/doc2.txt \
      (1) (4) hrg ks dso (hxx) (22) (gg (89)) [33] fhjd d www s a   saa s sa \
       sa sa sa de r  dhfhg dsmue "
  and doc3 = "/path/to/doc2.txt \
      (1) (4) hrg ks dso (hxx) (22) (gg (89)) [33] fhjd d www s a 777777 \
       yrgsv xxx222  saa s sa sa sa sa de r  \n\
       dhfhg dsmue gg foo bar 563 gggx fffffxx htx hml html xml css qqq aaa \
       222222 vvv htygr hdfgddds tom"
  and doc4 = "/path/to/doc2.txt \
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
       tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
       veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
       commodo consequat. Duis aute irure dolor in reprehenderit in voluptate \
       velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
       occaecat cupidatat non proident, sunt in culpa ..."
  and doc5 = "/path/to/doc2.txt \
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
       tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
       veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
       commodo consequat. Duis aute irure dolor in reprehenderit in voluptate \
       velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
       occaecat cupidatat non proident, sunt in culpa qui officia deserunt \
       mollit anim id est laborum. 10 11 12 13 14 15 16 17 18 19 20 21\n"
  in
  [
    (fun () -> ignore (Token.from_string doc0))
    |> Bench.Test.create ~name:"Parser: parse doc0 (empty)";
    (fun () -> ignore (Token.from_string doc1))
    |> Bench.Test.create ~name:"Parser: parse doc1 (only path)";
    (fun () -> ignore (Token.from_string doc2))
    |> Bench.Test.create ~name:"Parser: parse doc2 (20 tokens)";
    (fun () -> ignore (Token.from_string doc3))
    |> Bench.Test.create ~name:"Parser: parse doc3 (40 tokens)";
    (fun () -> ignore (Token.from_string doc4))
    |> Bench.Test.create ~name:"Parser: parse doc4 (60 tokens)";
    (fun () -> ignore (Token.from_string doc5))
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
  Command_unix.run
    (Bench.make_command
       (index_benchmark index_path @ parser_benchmark))
