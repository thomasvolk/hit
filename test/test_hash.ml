open OUnit2
open Hit

let tests = [
  ( "create" >:: (fun _ ->
      let h = Hash.create "doc" "content" in
      assert_equal ~printer:Hash.to_string ("doc", "9a0364b9e99bb480dd25e1f0284c8555") h
  ));
  (
    "to_string and from_string" >:: (fun _ ->
      let h = Hash.create "token" "content" in
      let s = Hash.to_string h in
      let h' = Hash.from_string s in
      assert_equal ~printer:Hash.to_string h h'
    ));
  (
    "to_path and from_path" >:: (fun _ ->
      let h = Hash.create "doc" "content" in
      let path = Hash.to_path h in
      assert_equal ~printer:Fun.id "doc/9a/03/64/b9/e99bb480dd25e1f0284c8555" path;
      let h' = Hash.from_path path in
      assert_equal ~printer:Hash.to_string h h'
    ));
]

let _ = run_test_tt_main ("Hash" >::: tests)
