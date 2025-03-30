open OUnit2
open Hit

let print_token tk =
  let open Token in
  tk.token ^ "(pos=" ^ (Int.to_string tk.pos) ^ ")"

let print_token_list tks = tks |> List.map print_token |> String.concat ", "


let tests =
  "Token" >::: [
    "parse" >:: (fun _ ->
      let expected = [
        Token.create "test" 0;
        Token.create "foo"  6;
        Token.create "row2" 18;
        Token.create "row3" 24;
        Token.create "row3" 29;
        Token.create "5"    34;
        Token.create "row3" 38;
        Token.create "14"   43;
      ]
      in
      assert_equal ~printer:print_token_list expected (Token.parse "test (Foo)  . !\n\n ROW2\r\nrow3\trow3-5   rOw3-14");

      assert_equal ~printer:print_token_list [] (Token.parse " \n\n    ")
    )
  ]

  
let _ = 
  run_test_tt_main tests

