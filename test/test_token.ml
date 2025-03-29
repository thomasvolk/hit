open OUnit2
open Hit

let print_token tk =
  let open Token in
  tk.token ^ "(r=" ^ (Int.to_string tk.row) ^ ", c=" ^ (Int.to_string tk.col) ^ ")"

let print_token_list tks = tks |> List.map print_token |> String.concat ", "


let tests =
  "Token" >::: [
    "parse" >:: (fun _ ->
      let expected = [
        Token.create "test" 0 0;
        Token.create "foo"  0 6;
        Token.create "row2" 2 1;
        Token.create "row3" 3 0;
        Token.create "row3" 3 5;
        Token.create "5"    3 10;
        Token.create "row3" 3 14;
        Token.create "14"   3 19;
      ]
      in
      assert_equal ~printer:print_token_list expected (Token.parse "test (Foo)  . !\n\n ROW2\nrow3\trow3-5   rOw3-14");

      assert_equal ~printer:print_token_list [] (Token.parse " \n\n    ")
    )
  ]

  
let _ = 
  run_test_tt_main tests

