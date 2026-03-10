open Core_bench
open Hit

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
  Command_unix.run
    (Bench.make_command parser_benchmark)
