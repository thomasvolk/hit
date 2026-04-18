open OUnit2
open Hit.Score

let int_list_printer lst =
  let elements = String.concat "; " (List.map string_of_int lst) in
  Printf.sprintf "[%s]" elements

let tests =
  [
    ( "find_closest_numbers" >:: fun _ ->
      let rows = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] in
      let expected = [3; 4; 7] in
      let result = find_closest_numbers rows in
      assert_equal ~printer:int_list_printer expected result );
    ( "find_closest_numbers with empty input" >:: fun _ ->
      let rows = [] in
      let expected = [] in
      let result = find_closest_numbers rows in
      assert_equal ~printer:int_list_printer expected result );
    ( "find_closest_numbers with single row" >:: fun _ ->
      let rows = [[10; 20; 30]] in
      let expected = [10] in
      let result = find_closest_numbers rows in
      assert_equal ~printer:int_list_printer expected result );
  ]

let _ = run_test_tt_main ("Score" >::: tests)
