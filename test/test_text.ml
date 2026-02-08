open OUnit2
open Hit
open Text

let print_token_entry_list tks =
  tks |> List.map TokenEntry.to_string |> String.concat "\n"

let print_opt_int = function None -> "None" | Some i -> string_of_int i
let separators = String.to_seq Config.default_separators |> List.of_seq

let tests =
  "Text"
  >::: [
         ( "parse document" >:: fun _ ->
           assert_equal ~printer:print_token_entry_list []
             (Parser.parse separators
                (Document.create (Document.Meta.create "" "" "") " \n\n    "));
           let expected =
             [
               TokenEntry.create "dir" []
                 (TokenEntry.Flags.create false true false false);
               TokenEntry.create "foo" []
                 (TokenEntry.Flags.create true false false false);
               TokenEntry.create "local" []
                 (TokenEntry.Flags.create false false false true);
               TokenEntry.create "txt" []
                 (TokenEntry.Flags.create false false true false);
             ]
           in
           assert_equal ~printer:print_token_entry_list expected
             (Parser.parse separators
                (Document.create
                   (Document.Meta.create "local" "/dir/foo.txt" "")
                   " \n\n    "));
           let expected =
             [
               TokenEntry.create "14" [ 49 ] TokenEntry.Flags.empty;
               TokenEntry.create "documents" []
                 (TokenEntry.Flags.create false true false false);
               TokenEntry.create "first" [ 0 ] TokenEntry.Flags.empty;
               TokenEntry.create "foo" [ 12 ]
                 (TokenEntry.Flags.create true false false false);
               TokenEntry.create "local" []
                 (TokenEntry.Flags.create false false false true);
               TokenEntry.create "root" []
                 (TokenEntry.Flags.create false true false false);
               TokenEntry.create "row2" [ 24 ] TokenEntry.Flags.empty;
               TokenEntry.create "row3" [ 30; 35; 44 ] TokenEntry.Flags.empty;
               TokenEntry.create "test" [ 6 ]
                 (TokenEntry.Flags.create false true false false);
               TokenEntry.create "txt" []
                 (TokenEntry.Flags.create false false true false);
             ]
           in
           assert_equal ~printer:print_token_entry_list expected
             (Parser.parse separators
                (Document.create
                   (Document.Meta.create "local" "/root/test/documents/foo.txt"
                      "")
             "first test (Foo)  . !\n\n ROW2\r\nrow3\trow3/5   rOw3/14"));
           let expected =
             [
               TokenEntry.create "first" [ 0 ] TokenEntry.Flags.empty;
             ]
           in
           assert_equal ~printer:print_token_entry_list expected
             (Parser.parse separators
                (Document.create
                   (Document.Meta.create "x" "/x.x"
                      "") "first"))
             );
         ( "closest_distance" >:: fun _ ->
           let t1 =
             TokenEntry.create "t1" [ 43; 67; 100 ] TokenEntry.Flags.empty
           in
           let t2 =
             TokenEntry.create "t2" [ 10; 800; 1070 ] TokenEntry.Flags.empty
           in
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
