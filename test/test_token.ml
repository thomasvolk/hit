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
        Token.create "row2" 2 1;
        Token.create "row3" 3 0;
        Token.create "row3-5" 3 5;
        Token.create "row3-14" 3 14;
      ]
      in
      assert_equal ~printer:print_token_list expected (Token.parse "test \n\n row2\nrow3 row3-5   row3-14")
    )
  ]

  
let _ = 
  run_test_tt_main tests

