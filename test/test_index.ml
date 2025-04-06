open OUnit2
open Hit.Index

let print_string (s: string) = s

let tests =
  "Entry" >::: [
    "add" >:: (
      fun _ ->
        let e = Entry.empty
          |> Entry.add ("a01", [1; 2; 3])
          |> Entry.add ("a02", [4; 5; 6])
        in
        assert_equal ~printer:string_of_int 2 (Entry.size e);
    );
    "of_string" >:: (
      fun _ ->
        let e = Entry.of_string {|
        abc001 90 1   4  9
        abc002 1 7 90 66

        abc003

      |} in 
      assert_equal ~printer:string_of_int 3 (Entry.size e);
    );
    "to_string" >:: (
      fun _ ->
        let e = Entry.empty 
          |> Entry.add ("a01", [1; 2; 3])
          |> Entry.add ("a02", [4; 5; 6])
          |> Entry.add ("a03", [])
          |> Entry.add ("a04", [56; 8; 9; 19])
        in
        let expected = {|a01 1 2 3
a02 4 5 6
a03
a04 56 8 9 19
|} in
        assert_equal ~printer:print_string expected (Entry.to_string e);
    )
  ]

let _ = 
  run_test_tt_main tests
