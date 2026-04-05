open OUnit2
open Hit

let tests =
  [
    ( "Index.add" >:: fun _ ->
      let test_dir = "./test_index" in
      Io.delete_all_files ~predicate:(fun _ -> true) test_dir;
      let idx = Index.create test_dir in
      let doc_id = Index.add idx "/tmp/doc01.txt" "Document 01 - 1 2 3" in
      assert_equal ~printer:Fun.id "doc-8cc92cb296ec50f6a41ea3d7711ca06f"
        (Doc.Id.to_string doc_id) );
  ]

let _ = run_test_tt_main ("Index" >::: tests)
