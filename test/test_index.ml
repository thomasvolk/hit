open OUnit2
open Hit.Index

let tests =
  "Entry" >::: [
    "add" >:: (
      fun _ ->
        let e = Entry.create in
        let e1 = Entry.add e ("a01", [1; 2; 3]) in
        let e2 = Entry.add e1 ("a02", [4; 5; 6]) in
        assert_equal ~printer:string_of_int 2 (Entry.size e2);
    );
    "of_string" >:: (
      fun _ ->
        let e = Entry.of_string {|
        abc001 90 1   4  9
        abc002 1 7 90 66

        abc003

      |} in 
      assert_equal ~printer:string_of_int 3 (Entry.size e);
    )
  ]

let _ = 
  run_test_tt_main tests
