open OUnit2
open Hit
open Analyzer

let print_entry_list tks = Core.Sexp.to_string (Core.List.sexp_of_t Entry.sexp_of_t tks)


let tests =
  "Token" >::: [
    "parse" >:: (fun _ ->
      let expected = [
        Entry.create "14"   [43];
        Entry.create "5"    [34];
        Entry.create "foo"  [6];
        Entry.create "row2" [18];
        Entry.create "row3" [38; 29; 24];
        Entry.create "test" [0];
      ]
      in
      assert_equal ~printer:print_entry_list expected (Parser.parse "test (Foo)  . !\n\n ROW2\r\nrow3\trow3-5   rOw3-14");

      assert_equal ~printer:print_entry_list [] (Parser.parse " \n\n    ")
    )
  ]

  
let _ = 
  run_test_tt_main tests

