open OUnit2
open Hit

let tests =
  [
    ( "Index.add" >:: fun _ ->
      let test_dir = "./test_index" in
      Io.delete_all_files ~predicate:(fun _ -> true) test_dir;
      let idx = Index.create test_dir in
      Index.add idx "/tmp/doc01.txt" "Document 01 - 1 2 3" );
  ]

let _ = run_test_tt_main ("Index" >::: tests)
