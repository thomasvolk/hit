open OUnit2
open Hit

let print_preview p =
  let open View.Preview in
  List.map (fun e -> match e with
    | Text t -> "Text(" ^ t ^ ")"
    | Token t -> "Token(" ^ t ^ ")"
  ) p 
    |> String.concat "|"

let tests =
  "Preview"
  >::: [
         ( "of_tokens" >:: fun _ ->
           let open View.Preview in
           let p = View.Preview.of_tokens "123 a1 456 b2 789" [("a1", 4); ("b2", 11)] in
           assert_equal ~printer:print_preview [Text "123 "; Token "a1"; Text " 456 "; Token "b2"; Text " 789"] p
         );
       ]

let _ = run_test_tt_main tests
