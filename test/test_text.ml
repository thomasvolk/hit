open OUnit2
open Hit
open Text

let print_entry_list tks =
  Core.Sexp.to_string (Core.List.sexp_of_t TokenEntry.sexp_of_t tks)

let tests =
  "Parser"
  >::: [
         ( "parse" >:: fun _ ->
           let expected =
             [
               TokenEntry.create "14" [ 43 ];
               TokenEntry.create "5" [ 34 ];
               TokenEntry.create "foo" [ 6 ];
               TokenEntry.create "row2" [ 18 ];
               TokenEntry.create "row3" [ 38; 29; 24 ];
               TokenEntry.create "test" [ 0 ];
             ]
           in
           assert_equal ~printer:print_entry_list expected
             (Parser.parse "test (Foo)  . !\n\n ROW2\r\nrow3\trow3-5   rOw3-14");

           assert_equal ~printer:print_entry_list [] (Parser.parse " \n\n    ")
         );
       ]

let _ = run_test_tt_main tests
