open OUnit2
open Hit
open Text

let print_entry_list tks =
  Core.Sexp.to_string (Core.List.sexp_of_t TokenEntry.sexp_of_t tks)

let print_opt_int = function None -> "None" | Some i -> string_of_int i

let tests =
  "Text"
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
         ( "closest_distance" >:: fun _ ->
           let t1 = TokenEntry.create "t1" [ 43; 67; 100 ] in
           let t2 = TokenEntry.create "t2" [ 10; 800; 1070 ] in
           assert_equal ~printer:print_opt_int (Some 33)
             (TokenEntry.closest_distance t1 t2
             |> Option.map Token.Distance.distance);
           let t1 = TokenEntry.create "t1" [] in
           let t2 = TokenEntry.create "t2" [] in
           assert_equal ~printer:print_opt_int None
             (TokenEntry.closest_distance t1 t2
             |> Option.map Token.Distance.distance);
           let t1 = TokenEntry.create "t1" [ 1 ] in
           let t2 = TokenEntry.create "t2" [] in
           assert_equal ~printer:print_opt_int None
             (TokenEntry.closest_distance t1 t2
             |> Option.map Token.Distance.distance);
           let t1 = TokenEntry.create "t1" [] in
           let t2 = TokenEntry.create "t2" [ 1 ] in
           assert_equal ~printer:print_opt_int None
             (TokenEntry.closest_distance t1 t2
             |> Option.map Token.Distance.distance) );
       ]

let _ = run_test_tt_main tests
