open OUnit2
open Hit

let print_string s = s

let tests =
  "Text" >::: [
    "normalize" >:: (fun _ ->
      assert_equal ~printer:print_string  "1\n2\n3\n4\n5" (Text.normalize_linefeed "1\r\n2\n\r3\r4\n5");
    )
  ]

  
let _ = 
  run_test_tt_main tests

