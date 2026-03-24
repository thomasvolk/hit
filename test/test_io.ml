open OUnit2
open Hit.Io

let tests =
  [
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
  ]

let _ = run_test_tt_main ("Io" >::: tests)
