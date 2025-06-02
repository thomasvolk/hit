open OUnit2
open Hit


let test_path = "./test_io/index"
module Storage = (val (Io.file_storage test_path) :  Io.StorageInstance)


let tests =
  "Storage" >::: [
    "Doc_table" >:: (
      fun _ ->
        let r = (Ref.create "test") in
        let ti = Storage.Impl.load_doc_table r Storage.t 
                  |> Doc_table.add (Ref.create "notes::main.md") [1; 2; 3]
                  |> Doc_table.add (Ref.create "notes::x.md") [8; 23; 89]
                  |> Doc_table.add (Ref.create "notes::a/b/foo.md") [34; 200; 387] in
        Storage.Impl.save_doc_table r ti Storage.t;
        let expected = {|3f61a33051c00c43956ca8b798ca651e 8 23 89
58bc212a2d19e9b88ec655e5d2194dd7 34 200 387
e4fb6111620be10611cf5a25e38339d4 1 2 3
|} in
        assert_equal ~printer:Fun.id expected (Io.read_file (Filename.concat test_path "doc-table/09/8f/6b/cd/4621d373cade4e832627b4f6"));
        let dt' = Storage.Impl.load_doc_table r Storage.t in
        assert_equal 3 (Doc_table.size dt')
    );
    "Term_table" >:: (
      fun _ ->
        let tt = Storage.Impl.load_term_table Storage.t
          |> Term_table.add "test" (Ref.create "dt01")
          |> Term_table.add "foo" (Ref.create "dt02")
          |> Term_table.add "x" (Ref.create "dt03")
        in
        Storage.Impl.save_term_table tt Storage.t;
        let expected = {|foo 24bb721b911892725b6fa345dcae7bd7
test 01bda9acfa61a60264bce1d59c60c77b
x 536f8f0a0ff495390bd37e6521dbdb9d
|} in
        assert_equal ~printer:Fun.id expected (Io.read_file (Filename.concat test_path "term-table"));
        let tt' = Storage.Impl.load_term_table Storage.t in
        assert_equal (Some (Ref.create "dt03")) (Term_table.get "x" tt');
        assert_equal 3 (Term_table.size tt')
    );
  ]

let _ = 
  run_test_tt_main tests
