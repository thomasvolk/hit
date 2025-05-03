open OUnit2
open Hit

module TermIndexIo = Storage.Make(Storage.TermIndexFile)

let add_entry r l ti =
  let open Hit.Index in
  TermIndex.add (TermIndex.Entry.create (Ref.create r) l) ti 

let test_path = "./test_io/index"

let tests =
  "Storage" >::: [
    "load" >:: (
      fun _ ->
        let cfg = Storage.TermIndexFile.create test_path in
        let ti = TermIndexIo.load "test" cfg 
        |> add_entry "notes::main.md" [1; 2; 3]
        |> add_entry "notes::x.md" [8; 23; 89]
        |> add_entry "notes::a/b/foo.md" [34; 200; 387] in
        TermIndexIo.save ti cfg;
        let expected = {|3f61a33051c00c43956ca8b798ca651e 8 23 89
58bc212a2d19e9b88ec655e5d2194dd7 34 200 387
e4fb6111620be10611cf5a25e38339d4 1 2 3
|} in
        assert_equal ~printer:Fun.id expected (Storage.read_file (Filename.concat test_path "term-index/09/8f/6b/cd/4621d373cade4e832627b4f6"));
    );
  ]

let _ = 
  run_test_tt_main tests
