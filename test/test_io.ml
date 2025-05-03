open OUnit2

let tests =
  "Io" >::: [
    "load" >:: (
      fun _ ->
        assert_equal ~printer:Fun.id ""  ""
    );
  ]

let _ = 
  run_test_tt_main tests
