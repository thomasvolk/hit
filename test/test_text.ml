open OUnit2
open Hit
open Text

let print_entry_list l = List.map (fun (w, pl) -> w ^ (List.map string_of_int pl |> String.concat " ")) l |> String.concat ", "

let print_token_entry_list tks =
  Core.Sexp.to_string (Core.List.sexp_of_t TokenEntry.sexp_of_t tks)
  
let print_opt_int = function None -> "None" | Some i -> string_of_int i

let separators = String.to_seq Config.default_separators |> List.of_seq

let tests =
  "Text"
  >::: [
         ( "parse string" >:: fun _ ->
           let expected =
             [
               ("14", [ 43 ] );
               ("foo", [ 6 ] );
               ("row2", [ 18 ] );
               ("row3", [ 38; 29; 24 ] );
               ("test", [ 0 ] );
             ]
           in
           assert_equal ~printer:print_entry_list expected
             (Parser.parse_string separators
                "test (Foo)  . !\n\n ROW2\r\nrow3\trow3/5   rOw3/14");

           assert_equal ~printer:print_entry_list []
             (Parser.parse_string separators " \n\n    ") );
         ( "parse document" >:: fun _ ->
           let expected =
             [
               TokenEntry.create "14" [ 43 ] TokenEntry.Flags.empty;
               TokenEntry.create "foo" [ 6 ] (TokenEntry.Flags.create true false false false);
               TokenEntry.create "row2" [ 18 ] TokenEntry.Flags.empty;
               TokenEntry.create "row3" [ 38; 29; 24 ] TokenEntry.Flags.empty;
               TokenEntry.create "test" [ 0 ] (TokenEntry.Flags.create false true false false);
             ]
           in
           assert_equal ~printer:print_token_entry_list expected
             (Parser.parse separators (Document.create (Document.Meta.create "local" "/root/test/documents/foo.txt" "")
                "test (Foo)  . !\n\n ROW2\r\nrow3\trow3/5   rOw3/14"));

           assert_equal ~printer:print_entry_list []
             (Parser.parse_string separators " \n\n    ") );
         ( "closest_distance" >:: fun _ ->
           let t1 = TokenEntry.create "t1" [ 43; 67; 100 ] TokenEntry.Flags.empty in
           let t2 = TokenEntry.create "t2" [ 10; 800; 1070 ] TokenEntry.Flags.empty in
           assert_equal ~printer:print_opt_int (Some 33)
             (TokenEntry.closest_distance t1 t2
             |> Option.map Text.TokenPair.distance_vec);
           let t1 = TokenEntry.create "t1" [] TokenEntry.Flags.empty in
           let t2 = TokenEntry.create "t2" [] TokenEntry.Flags.empty in
           assert_equal ~printer:print_opt_int None
             (TokenEntry.closest_distance t1 t2
             |> Option.map Text.TokenPair.distance_vec);
           let t1 = TokenEntry.create "t1" [ 1 ] TokenEntry.Flags.empty in
           let t2 = TokenEntry.create "t2" [] TokenEntry.Flags.empty in
           assert_equal ~printer:print_opt_int None
             (TokenEntry.closest_distance t1 t2
             |> Option.map Text.TokenPair.distance_vec);
           let t1 = TokenEntry.create "t1" [] TokenEntry.Flags.empty in
           let t2 = TokenEntry.create "t2" [ 1 ] TokenEntry.Flags.empty in
           assert_equal ~printer:print_opt_int None
             (TokenEntry.closest_distance t1 t2
             |> Option.map Text.TokenPair.distance_vec) );
       ]

let _ = run_test_tt_main tests
