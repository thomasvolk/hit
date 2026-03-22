open OUnit2
open Hit

let tests =
  [
    ( "Trx.execute" >:: fun _ ->
      let test_dir = "./test_io_trx" in
      Io.delete_all_files ~predicate:(fun _ -> true) test_dir;
      let trx =
        Io.Trx.empty
        |> Io.Trx.add
             (Io.Action.of_write_file
                (Filename.concat test_dir "testfile01.txt")
                (Core.Sexp.of_string "TEST01"))
        |> Io.Trx.add
             (Io.Action.of_write_file
                (Filename.concat test_dir "testfile02.txt")
                (Core.Sexp.of_string "TEST02"))
      in
      Io.execute_transaction (Filename.concat test_dir "trx.hit") trx );
    ( "Trx.retry" >:: fun _ ->
      let test_dir = "./test_io_trx" in
      Io.delete_all_files ~predicate:(fun _ -> true) test_dir;
      let trx =
        Io.Trx.empty
        |> Io.Trx.add
             (Io.Action.of_write_file
                (Filename.concat test_dir "testfile01.txt")
                (Core.Sexp.of_string "TEST01"))
        |> Io.Trx.add
             (Io.Action.of_delete_file
                (Filename.concat test_dir "testfile03.txt"))
      in
      let trx_path = Filename.concat test_dir "trx.hit" in
      (* write the transaction to the file system *)
      Io.write_file_from_sexp trx_path (Io.Trx.sexp_of_t trx);
      (* try to execute the transaction *)
      assert_raises
        (Io.TransactionError "transaction already exists: ./test_io_trx/trx.hit")
        (fun () -> Io.execute_transaction trx_path trx);
      (* read the transaction from file *)
      let trx' = Io.Trx.t_of_sexp (Io.read_file_to_sexp trx_path) in
      (* execute the transaction without check *)
      Io.execute_transaction ~check_for_existing:false trx_path trx' );
  ]

let _ = run_test_tt_main ("Io" >::: tests)
