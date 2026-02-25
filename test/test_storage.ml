open OUnit2
open Hit
open Hit.Text.TokenEntry

let test_path = "./test_storage_" ^ string_of_float (Unix.gettimeofday ())

module FileStorage = (val Io.file_storage test_path : Io.StorageInstance)

module InMemoryStorage =
  (val Io.in_memory_storage (200, 200) : Io.StorageInstance)

let tests (module Storage : Io.StorageInstance) =
  [
    ( "Table.TokenTable" >:: fun _ ->
      let tt =
        FileStorage.Impl.load_token_table FileStorage.t
        |> Table.TokenTable.add "test" (Table.DocumentTable.Id.create "dt01")
        |> Table.TokenTable.add "foo" (Table.DocumentTable.Id.create "dt02")
        |> Table.TokenTable.add "x" (Table.DocumentTable.Id.create "dt03")
      in
      FileStorage.Impl.save_token_table tt FileStorage.t;
      let expected =
        {|foo dtb-24bb721b911892725b6fa345dcae7bd7
test dtb-01bda9acfa61a60264bce1d59c60c77b
x dtb-536f8f0a0ff495390bd37e6521dbdb9d
|}
      in
      assert_equal ~printer:Fun.id expected
        (Io.read_file (Filename.concat test_path "token-table"));
      let tt' = FileStorage.Impl.load_token_table FileStorage.t in
      assert_equal
        (Some (Table.DocumentTable.Id.create "dt03"))
        (Table.TokenTable.get "x" tt');
      assert_equal 3 (Table.TokenTable.size tt') );
    ( "Table.DocumentTable" >:: fun _ ->
      let dt_id = Table.DocumentTable.Id.create "test" in
      let dt =
        FileStorage.Impl.load_doc_table dt_id FileStorage.t
        |> Table.DocumentTable.add
             (Document.Id.create "notes::main.md")
             (Flags.empty, [ 1; 2; 3 ])
        |> Table.DocumentTable.add
             (Document.Id.create "notes::x.md")
             (Flags.create true true true true, [ 8; 23; 89 ])
        |> Table.DocumentTable.add
             (Document.Id.create "notes::a/b/foo.md")
             (Flags.empty |> Flags.set_directory, [ 34; 200; 387 ])
      in
      FileStorage.Impl.save_doc_table dt FileStorage.t;
      let expected =
        {|doc-3f61a33051c00c43956ca8b798ca651e TDES 8 23 89
doc-58bc212a2d19e9b88ec655e5d2194dd7 D 34 200 387
doc-e4fb6111620be10611cf5a25e38339d4  1 2 3
|}
      in
      assert_equal ~printer:Fun.id expected
        (Io.read_file
           (Filename.concat test_path "dtb/09/8f/6b/cd/4621d373cade4e832627b4f6"));
      let dt' = FileStorage.Impl.load_doc_table dt_id FileStorage.t in
      assert_equal 3 (Table.DocumentTable.size dt') );
    ( "Table.Document" >:: fun _ ->
      let d =
        Document.from_source "local" "my-notes/note.md" "this is my note"
      in
      FileStorage.Impl.save_doc d FileStorage.t;
      let d_id = Document.id d in
      let d' = FileStorage.Impl.load_doc d_id FileStorage.t in
      assert_equal d d' );
    ( "Table.DocumentRegister" >:: fun _ ->
      let dr =
        FileStorage.Impl.load_doc_register FileStorage.t
        |> Table.DocumentRegister.add
             (Document.Id.create "notes::main.md")
        |> Table.DocumentRegister.add
             (Document.Id.create "notes::x.md")
        |> Table.DocumentRegister.add
             (Document.Id.create "notes::a/b/foo.md")
      in
      FileStorage.Impl.save_doc_register dr FileStorage.t;
      let expected =
        {|doc-3f61a33051c00c43956ca8b798ca651e
doc-58bc212a2d19e9b88ec655e5d2194dd7
doc-e4fb6111620be10611cf5a25e38339d4
|}
      in
      assert_equal ~printer:Fun.id expected
        (Io.read_file (Filename.concat test_path "doc-register"));
      let dr' = FileStorage.Impl.load_doc_register FileStorage.t in
      assert_equal 3 (Table.DocumentRegister.size dr') );
    ( "Lock/Unlock" >:: fun _ ->
      FileStorage.Impl.lock ~force:true FileStorage.t;
      FileStorage.Impl.lock ~force:true FileStorage.t;
      let expected_lock =
        Unix.Unix_error (Unix.EEXIST, "open", test_path ^ "/lock")
      in
      assert_raises expected_lock (fun () ->
          FileStorage.Impl.lock FileStorage.t);
      FileStorage.Impl.unlock FileStorage.t;
      let expected_unlock =
        Unix.Unix_error (Unix.ENOENT, "unlink", test_path ^ "/lock")
      in
      assert_raises expected_unlock (fun () ->
          FileStorage.Impl.unlock FileStorage.t);
      FileStorage.Impl.lock FileStorage.t;
      FileStorage.Impl.unlock FileStorage.t );
    ( "get_all_doc_ids" >:: fun _ ->
      let dl =
        [ "A"; "B"; "C"; "D" ]
        |> List.map (fun i ->
            Document.from_source "local"
              ("my-notes/note" ^ i ^ ".md")
              ("this is my note" ^ i))
        |> List.map (fun d -> FileStorage.Impl.save_doc d FileStorage.t)
      in
      let did_set = FileStorage.Impl.get_all_doc_ids FileStorage.t in
      assert_bool "list of document must be equal or or greater"
        (List.length (Io.DocumentIdSet.elements did_set) >= List.length dl) );
  ]

let _ =
  run_test_tt_main
    ("Storage" >::: tests (module FileStorage) @ tests (module InMemoryStorage))
