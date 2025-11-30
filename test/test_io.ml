open OUnit2
open Hit

let tests =
  "Io"
  >::: [
         ( "hash / path" >:: fun _ ->
           let h = Reference.hash "123" in
           let p = Io.hash_to_path h in
           assert_equal ~printer:Fun.id "20/2c/b9/62/ac59075b964b07152d234b70" p;
           assert_equal ~printer:Fun.id h (Io.path_to_hash p) );
       ]

let _ = run_test_tt_main tests
