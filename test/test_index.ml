open OUnit2
open Hit.Index

let print_string (s: string) = s

let tests =
  "Register" >::: [
    "add" >:: (
      fun _ ->
        let e = Register.empty "test"
          |> Register.add (Register.Ref.create "a01" [1; 2; 3])
          |> Register.add (Register.Ref.create "a02" [4; 5; 6])
        in
        assert_equal ~printer:string_of_int 2 (Register.size e);
    );
    "of_string" >:: (
      fun _ ->
        let e = Register.of_string {|test
        abc001 90 1   4  9
        abc002 1 7 90 66

        abc004 0

      |} in 
      assert_equal ~printer:string_of_int 3 (Register.size e);
    );
    "to_string" >:: (
      fun _ ->
        let e = Register.empty "test"
          |> Register.add (Register.Ref.create "a01" [1; 2; 3])
          |> Register.add (Register.Ref.create "a02" [4; 5; 6])
          |> Register.add (Register.Ref.create "a03" [6])
          |> Register.add (Register.Ref.create "a04" [56; 8; 9; 19])
        in
        let expected = {|test
a01 1 2 3
a02 4 5 6
a03 6
a04 56 8 9 19
|} in
        assert_equal ~printer:print_string expected (Register.to_string e);
    );
    "invalid ref" >:: (
      fun _ ->
        assert_raises (Register.Ref.InvalidRef "position list is empty") (fun () -> Register.Ref.create "123" []);
    )
  ]

let _ = 
  run_test_tt_main tests
