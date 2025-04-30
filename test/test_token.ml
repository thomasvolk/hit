open OUnit2
open Hit

let print_token_list tks = Core.Sexp.to_string (Core.List.sexp_of_t Token.sexp_of_t tks)


let tests =
  "Token" >::: [
    "parse" >:: (fun _ ->
      let expected = [
        Token.create "14"   [43];
        Token.create "5"    [34];
        Token.create "foo"  [6];
        Token.create "row2" [18];
        Token.create "row3" [38; 29; 24];
        Token.create "test" [0];
      ]
      in
      assert_equal ~printer:print_token_list expected (Token.Parser.parse "test (Foo)  . !\n\n ROW2\r\nrow3\trow3-5   rOw3-14");

      assert_equal ~printer:print_token_list [] (Token.Parser.parse " \n\n    ")
    )
  ]

  
let _ = 
  run_test_tt_main tests

