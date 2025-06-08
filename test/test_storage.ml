open OUnit2
open Hit


let test_path = "./test_io/index"
module Storage = (val (Io.file_storage test_path) :  Io.StorageInstance)


let tests =
  "Storage" >::: [
    "Model.TokenTable" >:: (
      fun _ ->
        let tt = Storage.Impl.load_token_table Storage.t
          |> Model.TokenTable.add "test" (Model.DocumentTable.Id.create "dt01")
          |> Model.TokenTable.add "foo" (Model.DocumentTable.Id.create "dt02")
          |> Model.TokenTable.add "x" (Model.DocumentTable.Id.create "dt03")
        in
        Storage.Impl.save_token_table tt Storage.t;
        let expected = {|foo dtb-24bb721b911892725b6fa345dcae7bd7
test dtb-01bda9acfa61a60264bce1d59c60c77b
x dtb-536f8f0a0ff495390bd37e6521dbdb9d
|} in
        assert_equal ~printer:Fun.id expected (Io.read_file (Filename.concat test_path "term-table"));
        let tt' = Storage.Impl.load_token_table Storage.t in
        assert_equal (Some (Model.DocumentTable.Id.create "dt03")) (Model.TokenTable.get "x" tt');
        assert_equal 3 (Model.TokenTable.size tt')
    );
    "Model.DocumentTable" >:: (
      fun _ ->
        let dt_id = (Model.DocumentTable.Id.create "test") in
        let dt = Storage.Impl.load_doc_table dt_id Storage.t 
                  |> Model.DocumentTable.add (Model.Document.Id.create "notes::main.md") [1; 2; 3]
                  |> Model.DocumentTable.add (Model.Document.Id.create "notes::x.md") [8; 23; 89]
                  |> Model.DocumentTable.add (Model.Document.Id.create "notes::a/b/foo.md") [34; 200; 387] in
        Storage.Impl.save_doc_table dt Storage.t;
        let expected = {|doc-3f61a33051c00c43956ca8b798ca651e 8 23 89
doc-58bc212a2d19e9b88ec655e5d2194dd7 34 200 387
doc-e4fb6111620be10611cf5a25e38339d4 1 2 3
|} in
        assert_equal ~printer:Fun.id expected (Io.read_file (Filename.concat test_path "dtb/09/8f/6b/cd/4621d373cade4e832627b4f6"));
        let dt' = Storage.Impl.load_doc_table dt_id Storage.t in
        assert_equal 3 (Model.DocumentTable.size dt')
    );
    "Model.Document" >:: (
      fun _ ->
        let d = Model.Document.create (Model.Document.Meta.create "local" "my-notes/note.md") "this is my note" in
        Storage.Impl.save_doc d Storage.t;
        let d_id = Model.Document.id d in
        let d' = Storage.Impl.load_doc d_id Storage.t in
        assert_equal d d'
    );
  ]

let _ = 
  run_test_tt_main tests
