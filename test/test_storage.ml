open OUnit2
open Hit


let test_path = "./test_io/index"
module DocTableStorage = (val (Io.doc_table_file_storage test_path) :  Io.StorageInstance with type v = Doc_table.t)


let tests =
  "Storage" >::: [
    "load" >:: (
      fun _ ->
        let r = (Ref.create "test") in
        let ti = DocTableStorage.Impl.load r DocTableStorage.t 
                  |> Doc_table.add (Ref.create "notes::main.md") [1; 2; 3]
                  |> Doc_table.add (Ref.create "notes::x.md") [8; 23; 89]
                  |> Doc_table.add (Ref.create "notes::a/b/foo.md") [34; 200; 387] in
        DocTableStorage.Impl.save r ti DocTableStorage.t;
        let expected = {|3f61a33051c00c43956ca8b798ca651e 8 23 89
58bc212a2d19e9b88ec655e5d2194dd7 34 200 387
e4fb6111620be10611cf5a25e38339d4 1 2 3
|} in
        assert_equal ~printer:Fun.id expected ( Io.read_file (Filename.concat test_path "doc-table/09/8f/6b/cd/4621d373cade4e832627b4f6"));
    );
  ]

let _ = 
  run_test_tt_main tests
