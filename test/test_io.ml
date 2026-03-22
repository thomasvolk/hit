open OUnit2
open Hit

let tests = [
  ( "Trx" >:: (fun _ ->
    let test_dir = "./test_io_trx" in
    Io.delete_all_files ~predicate:(fun _ -> true) test_dir;
    let trx = Io.Trx.empty
    |> Io.Trx.add (Io.Action.of_write_file (Filename.concat test_dir "testfile01.txt") (Core.Sexp.of_string "TEST01"))
    |> Io.Trx.add (Io.Action.of_write_file (Filename.concat test_dir "testfile02.txt") (Core.Sexp.of_string "TEST02"))
    in
    Io.execute_transaction (Filename.concat test_dir "trx.hit") trx
  ))
]

let _ = run_test_tt_main ("Io" >::: tests)
