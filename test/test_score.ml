open OUnit2
open Hit.Score

let int_list_printer = function
  | Some (lst, span) ->
      let elements = String.concat "; " (List.map string_of_int lst) in
      Printf.sprintf "[%s] span=%n" elements span
  | None -> "None"

let tests =
  [
    ( "find_closest_elements" >:: fun _ ->
      let rows = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] in
      let expected = Some ([ 3; 4; 7 ], 4) in
      let result = find_closest_elements rows in
      assert_equal ~printer:int_list_printer expected result );
    ( "find_closest_elements with empty input" >:: fun _ ->
      let rows = [] in
      let expected = None in
      let result = find_closest_elements rows in
      assert_equal ~printer:int_list_printer expected result );
    ( "find_closest_elements with single row" >:: fun _ ->
      let rows = [ [ 10; 20; 30 ] ] in
      let expected = Some ([ 10 ], 0) in
      let result = find_closest_elements rows in
      assert_equal ~printer:int_list_printer expected result );
  ]

let _ = run_test_tt_main ("Score" >::: tests)
