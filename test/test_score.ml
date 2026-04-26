open OUnit2
open Hit.Score
open Hit.Token

let closest_elements_printer = function
  | Some (lst, span) ->
      let elements = String.concat "; " (List.map string_of_int lst) in
      Printf.sprintf "[%s] span=%n" elements span
  | None -> "None"

let entry positions = DocumentEntry.of_list positions

let assert_float_eq ~msg expected actual =
  assert_equal ~msg ~printer:(Printf.sprintf "%.6f")
    ~cmp:(fun a b -> Float.abs (a -. b) < 1e-9)
    expected actual

let tests =
  [
    (* ----------------------------------------------------------------- *)
    (* find_closest_elements                                              *)
    (* ----------------------------------------------------------------- *)
    ( "find_closest_elements: three rows, picks tightest combination"
    >:: fun _ ->
      let rows = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] in
      assert_equal ~printer:closest_elements_printer
        (Some ([ 3; 4; 7 ], 4))
        (find_closest_elements rows) );
    ( "find_closest_elements: empty input returns None" >:: fun _ ->
      assert_equal ~printer:closest_elements_printer None
        (find_closest_elements []) );
    ( "find_closest_elements: single row — span is always 0" >:: fun _ ->
      assert_equal ~printer:closest_elements_printer
        (Some ([ 10 ], 0))
        (find_closest_elements [ [ 10; 20; 30 ] ]) );
    ( "find_closest_elements: two rows adjacent — span 1" >:: fun _ ->
      assert_equal ~printer:closest_elements_printer
        (Some ([ 4; 5 ], 1))
        (find_closest_elements [ [ 1; 4 ]; [ 5; 10 ] ]) );
    ( "find_closest_elements: tie broken by lexicographic order" >:: fun _ ->
      (* [1;2], [3;2] and [3;4] all span 1; [1;2] is smallest lexicographically *)
      assert_equal ~printer:closest_elements_printer
        (Some ([ 1; 2 ], 1))
        (find_closest_elements [ [ 1; 3 ]; [ 2; 4 ] ]) );
    ( "find_closest_elements: single row with one element — span 0" >:: fun _ ->
      assert_equal ~printer:closest_elements_printer
        (Some ([ 7 ], 0))
        (find_closest_elements [ [ 7 ] ]) );
    (* ----------------------------------------------------------------- *)
    (* score                                                              *)
    (* Formula: log(2 + total_count) * (1 + 1 / (1 + span))             *)
    (* ----------------------------------------------------------------- *)
    ( "score: single entry, single position — span 0, df = 1.0" >:: fun _ ->
      (* cnt=1, span=0 → log(3) * 2 *)
      let expected = log 3. *. 2. in
      assert_float_eq ~msg:"score" expected (score [ entry [ 0 ] ]) );
    ( "score: single entry, multiple positions — span 0 (single-term match)"
    >:: fun _ ->
      (* cnt=3, span=0 → log(5) * 2 *)
      let expected = log 5. *. 2. in
      assert_float_eq ~msg:"score" expected (score [ entry [ 0; 1; 2 ] ]) );
    ( "score: two entries, adjacent positions — span 1" >:: fun _ ->
      (* cnt=2, span=1 → log(4) * (1 + 1/2) = log(4) * 1.5 *)
      let expected = log 4. *. 1.5 in
      assert_float_eq ~msg:"score" expected (score [ entry [ 0 ]; entry [ 1 ] ])
    );
    ( "score: two entries, far apart — larger span lowers score" >:: fun _ ->
      (* cnt=2, span=9 → log(4) * (1 + 1/10) = log(4) * 1.1 *)
      let expected = log 4. *. 1.1 in
      assert_float_eq ~msg:"score" expected (score [ entry [ 0 ]; entry [ 9 ] ])
    );
    ( "score: higher count ranks above lower count at same span" >:: fun _ ->
      let high_count = score [ entry [ 0; 1; 2; 3; 4 ] ] in
      let low_count = score [ entry [ 0 ] ] in
      assert_bool "higher count → higher score" (high_count > low_count) );
    ( "score: closer span ranks above wider span at same count" >:: fun _ ->
      let close = score [ entry [ 0 ]; entry [ 1 ] ] in
      let far = score [ entry [ 0 ]; entry [ 10 ] ] in
      assert_bool "closer span → higher score" (close > far) );
    ( "score: empty entry list — no positions, df = 0" >:: fun _ ->
      (* cnt=0, df=0 → log(2) * 1 *)
      let expected = log 2. *. 1. in
      assert_float_eq ~msg:"score" expected (score []) );
  ]

let _ = run_test_tt_main ("Score" >::: tests)
