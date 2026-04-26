open OUnit2
open Hit.Io

let setup dir =
  (try delete_all_files ~predicate:(fun _ -> true) dir with _ -> ());
  dir

let tests =
  [
    (* ----------------------------------------------------------------- *)
    (* Trx.execute / Trx.retry                                           *)
    (* ----------------------------------------------------------------- *)
    ( "Trx.execute" >:: fun _ ->
      let test_dir = "./test_io_trx" in
      delete_all_files ~predicate:(fun _ -> true) test_dir;
      let trx =
        Trx.empty
        |> Trx.add_write_file
             (test_dir // "testfile01.txt")
             (Core.Sexp.of_string "TEST01")
        |> Trx.add_write_file
             (test_dir // "testfile02.txt")
             (Core.Sexp.of_string "TEST02")
      in
      execute_transaction (Filename.concat test_dir "trx.hit") trx );
    ( "Trx.retry" >:: fun _ ->
      let test_dir = "./test_io_trx" in
      delete_all_files ~predicate:(fun _ -> true) test_dir;
      let trx =
        Trx.empty
        |> Trx.add_write_file
             (test_dir // "testfile01.txt")
             (Core.Sexp.of_string "TEST01")
        |> Trx.add_delete_file (test_dir // "testfile03.txt")
      in
      let trx_path = Filename.concat test_dir "trx.hit" in
      (* write the transaction to the file system *)
      write_file_from_sexp trx_path (Trx.sexp_of_t trx);
      (* try to execute the transaction *)
      assert_raises
        (TransactionError "transaction already exists: ./test_io_trx/trx.hit")
        (fun () -> execute_transaction trx_path trx);
      (* read the transaction from file *)
      let trx' = Trx.t_of_sexp (read_file_to_sexp trx_path) in
      (* execute the transaction without check *)
      execute_transaction ~check_for_existing:false trx_path trx' );
    (* ----------------------------------------------------------------- *)
    (* path operator                                                     *)
    (* ----------------------------------------------------------------- *)
    ( "//: concatenates path segments" >:: fun _ ->
      assert_equal ~printer:Fun.id "a/b/c" ("a" // "b" // "c") );
    (* ----------------------------------------------------------------- *)
    (* write / read roundtrip                                            *)
    (* ----------------------------------------------------------------- *)
    ( "write_file_from_sexp / read_file_to_sexp: roundtrip" >:: fun _ ->
      let dir = setup "./test_io_rw" in
      let path = dir // "data.hit" in
      let sexp = Core.Sexp.of_string "(hello world 42)" in
      write_file_from_sexp path sexp;
      let result = read_file_to_sexp path in
      assert_equal ~printer:Core.Sexp.to_string sexp result );
    ( "write_file_from_sexp: creates intermediate directories" >:: fun _ ->
      let dir = setup "./test_io_mkdir" in
      let path = dir // "a" // "b" // "c" // "deep.hit" in
      write_file_from_sexp path (Core.Sexp.of_string "deep");
      assert_bool "file exists" (file_exists path) );
    (* ----------------------------------------------------------------- *)
    (* file_exists                                                       *)
    (* ----------------------------------------------------------------- *)
    ( "file_exists: returns true for existing file" >:: fun _ ->
      let dir = setup "./test_io_exists" in
      let path = dir // "f.hit" in
      write_file_from_sexp path (Core.Sexp.of_string "x");
      assert_bool "file should exist" (file_exists path) );
    ( "file_exists: returns false for missing file" >:: fun _ ->
      assert_bool "should not exist" (not (file_exists "./no_such_file_xyz")) );
    (* ----------------------------------------------------------------- *)
    (* delete_file                                                       *)
    (* ----------------------------------------------------------------- *)
    ( "delete_file: removes an existing file" >:: fun _ ->
      let dir = setup "./test_io_del" in
      let path = dir // "gone.hit" in
      write_file_from_sexp path (Core.Sexp.of_string "x");
      delete_file path;
      assert_bool "file should be gone" (not (file_exists path)) );
    ( "delete_file: is a no-op on a missing file" >:: fun _ ->
      delete_file "./test_io_no_such_file_xyz.hit" );
    (* ----------------------------------------------------------------- *)
    (* find_all_files                                                    *)
    (* ----------------------------------------------------------------- *)
    ( "find_all_files: returns files matching predicate" >:: fun _ ->
      let dir = setup "./test_io_find" in
      write_file_from_sexp (dir // "a.hit") (Core.Sexp.of_string "a");
      write_file_from_sexp (dir // "b.hit") (Core.Sexp.of_string "b");
      write_file_from_sexp (dir // "c.txt") (Core.Sexp.of_string "c");
      let hits =
        find_all_files ~predicate:(fun f -> Filename.check_suffix f ".hit") dir
        |> List.length
      in
      assert_equal ~printer:string_of_int 2 hits );
    ( "find_all_files: recurses into subdirectories" >:: fun _ ->
      let dir = setup "./test_io_recurse" in
      write_file_from_sexp (dir // "top.hit") (Core.Sexp.of_string "t");
      write_file_from_sexp
        (dir // "sub" // "deep.hit")
        (Core.Sexp.of_string "d");
      let hits =
        find_all_files ~predicate:(fun f -> Filename.check_suffix f ".hit") dir
        |> List.length
      in
      assert_equal ~printer:string_of_int 2 hits );
    ( "find_all_files: does not return directories" >:: fun _ ->
      let dir = setup "./test_io_nodir" in
      write_file_from_sexp (dir // "sub" // "f.hit") (Core.Sexp.of_string "x");
      let results = find_all_files ~predicate:(fun _ -> true) dir in
      List.iter
        (fun f ->
          assert_bool (f ^ " must not be a dir") (not (Sys.is_directory f)))
        results );
    ( "find_all_files: returns empty list when no files match" >:: fun _ ->
      let dir = setup "./test_io_nomatch" in
      write_file_from_sexp (dir // "f.txt") (Core.Sexp.of_string "x");
      let results =
        find_all_files ~predicate:(fun f -> Filename.check_suffix f ".hit") dir
      in
      assert_equal ~printer:string_of_int 0 (List.length results) );
    (* ----------------------------------------------------------------- *)
    (* execute_transaction                                                *)
    (* ----------------------------------------------------------------- *)
    ( "execute_transaction: written files are present after commit" >:: fun _ ->
      let dir = setup "./test_io_trx_write" in
      let p1 = dir // "one.hit" and p2 = dir // "two.hit" in
      let trx =
        Trx.empty
        |> Trx.add_write_file p1 (Core.Sexp.of_string "ONE")
        |> Trx.add_write_file p2 (Core.Sexp.of_string "TWO")
      in
      execute_transaction (dir // "trx.hit") trx;
      assert_bool "one.hit exists" (file_exists p1);
      assert_bool "two.hit exists" (file_exists p2) );
    ( "execute_transaction: transaction log is removed after commit" >:: fun _ ->
      let dir = setup "./test_io_trx_log" in
      let trx_path = dir // "trx.hit" in
      let trx =
        Trx.empty
        |> Trx.add_write_file (dir // "f.hit") (Core.Sexp.of_string "x")
      in
      execute_transaction trx_path trx;
      assert_bool "trx log should be gone" (not (file_exists trx_path)) );
    ( "execute_transaction: delete action removes target file" >:: fun _ ->
      let dir = setup "./test_io_trx_del" in
      let target = dir // "victim.hit" in
      write_file_from_sexp target (Core.Sexp.of_string "x");
      let trx = Trx.empty |> Trx.add_delete_file target in
      execute_transaction (dir // "trx.hit") trx;
      assert_bool "victim should be gone" (not (file_exists target)) );
  ]

let _ = run_test_tt_main ("Io" >::: tests)
