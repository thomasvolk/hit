open OUnit2
open Hit

let print_string (s: string) = s

let tests =
  "Index" >::: [
    "add" >:: (
      fun _ ->
        let e = Index.empty
          |> Index.add (Index.Entry.create "a01" [1; 2; 3])
          |> Index.add (Index.Entry.create "a02" [4; 5; 6])
        in
        assert_equal ~printer:string_of_int 2 (Index.size e);
    );
    "of_string" >:: (
      fun _ ->
        let e = Index.of_string {|
        abc001 90 1   4  9
        abc002 1 7 90 66

        abc004 0

      |} in 
      assert_equal ~printer:string_of_int 3 (Index.size e);
    );
    "to_string" >:: (
      fun _ ->
        let e = Index.empty
          |> Index.add (Index.Entry.create "a01" [1; 2; 3])
          |> Index.add (Index.Entry.create "a02" [4; 5; 6])
          |> Index.add (Index.Entry.create "a03" [6])
          |> Index.add (Index.Entry.create "a04" [56; 8; 9; 19])
        in
        let expected = {|a01 1 2 3
a02 4 5 6
a03 6
a04 56 8 9 19
|} in
        assert_equal ~printer:print_string expected (Index.to_string e);
    );
    "invalid ref" >:: (
      fun _ ->
        assert_raises (Index.Entry.InvalidRef "position list is empty") (fun () -> Index.Entry.create "123" []);
    )
  ]

let _ = 
  run_test_tt_main tests
