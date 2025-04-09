open OUnit2
open Hit.Index

let print_string (s: string) = s

let tests =
  "Hash" >::: [
    "create" >:: (
      fun _ ->
        assert_equal ~printer:print_string "7215ee9c7d9dc229d2921a40e899ec5f" (Hash.create " ")
    );
    "path" >:: (
      fun _ ->
        let h = Hash.create " " in
        assert_equal ~printer:print_string "72/15/ee/9c/7d9dc229d2921a40e899ec5f" (Hash.to_path h)
    );
  ]

let _ = 
  run_test_tt_main tests
