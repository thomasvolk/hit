open OUnit2
open Hit
open Hit.Text.TokenEntry

let test_path = "./test_io/index"

module Storage = (val Io.file_storage test_path : Io.StorageInstance)

let tests =
  "Storage"
  >::: [
         ( "Table.TokenTable" >:: fun _ ->
           let tt =
             Storage.Impl.load_token_table Storage.t
             |> Table.TokenTable.add "test"
                  (Table.DocumentTable.Id.create "dt01")
             |> Table.TokenTable.add "foo"
                  (Table.DocumentTable.Id.create "dt02")
             |> Table.TokenTable.add "x" (Table.DocumentTable.Id.create "dt03")
           in
           Storage.Impl.save_token_table tt Storage.t;
           let expected =
             {|foo dtb-24bb721b911892725b6fa345dcae7bd7
test dtb-01bda9acfa61a60264bce1d59c60c77b
x dtb-536f8f0a0ff495390bd37e6521dbdb9d
|}
           in
           assert_equal ~printer:Fun.id expected
             (Io.read_file (Filename.concat test_path "token-table"));
           let tt' = Storage.Impl.load_token_table Storage.t in
           assert_equal
             (Some (Table.DocumentTable.Id.create "dt03"))
             (Table.TokenTable.get "x" tt');
           assert_equal 3 (Table.TokenTable.size tt') );
         ( "Table.DocumentTable" >:: fun _ ->
           let dt_id = Table.DocumentTable.Id.create "test" in
           let dt =
             Storage.Impl.load_doc_table dt_id Storage.t
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
           Storage.Impl.save_doc_table dt Storage.t;
           let expected =
             {|doc-3f61a33051c00c43956ca8b798ca651e TDES 8 23 89
doc-58bc212a2d19e9b88ec655e5d2194dd7 D 34 200 387
doc-e4fb6111620be10611cf5a25e38339d4  1 2 3
|}
           in
           assert_equal ~printer:Fun.id expected
             (Io.read_file
                (Filename.concat test_path
                   "dtb/09/8f/6b/cd/4621d373cade4e832627b4f6"));
           let dt' = Storage.Impl.load_doc_table dt_id Storage.t in
           assert_equal 3 (Table.DocumentTable.size dt') );
         ( "Table.Document" >:: fun _ ->
           let d =
             Document.from_source "local" "my-notes/note.md" "this is my note"
           in
           Storage.Impl.save_doc d Storage.t;
           let d_id = Document.id d in
           let d' = Storage.Impl.load_doc d_id Storage.t in
           assert_equal d d' );
         ( "Lock/Unlock" >:: fun _ ->
           Storage.Impl.lock ~force:true Storage.t;
           Storage.Impl.lock ~force:true Storage.t;
           let expected_lock =
             Unix.Unix_error (Unix.EEXIST, "open", "./test_io/index/lock")
           in
           assert_raises expected_lock (fun () -> Storage.Impl.lock Storage.t);
           Storage.Impl.unlock Storage.t;
           let expected_unlock =
             Unix.Unix_error (Unix.ENOENT, "unlink", "./test_io/index/lock")
           in
           assert_raises expected_unlock (fun () ->
               Storage.Impl.unlock Storage.t);
           Storage.Impl.lock Storage.t;
           Storage.Impl.unlock Storage.t );
         ( "get_all_doc_ids" >:: fun _ ->
           let dl =
             [ "A"; "B"; "C"; "D" ]
             |> List.map (fun i ->
                    Document.from_source "local"
                      ("my-notes/note" ^ i ^ ".md")
                      ("this is my note" ^ i))
             |> List.map (fun d -> Storage.Impl.save_doc d Storage.t)
           in
           let did_set = Storage.Impl.get_all_doc_ids Storage.t in
           assert_bool "list of document must be equal or or greater"
             (List.length (Io.DocumentIdSet.elements did_set) >= List.length dl)
         );
       ]

let _ = run_test_tt_main tests
