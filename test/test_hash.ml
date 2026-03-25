open OUnit2
open Hit

module Id = Hash.Make (struct
  let prefix = "hash"
end)

let tests =
  [
    ( "create" >:: fun _ ->
      let h = Id.create "content" in
      assert_equal ~printer:Fun.id "hash-9a0364b9e99bb480dd25e1f0284c8555"
        (Id.to_string h) );
    ( "to_string and from_string" >:: fun _ ->
      let h = Id.create "content" in
      let s = Id.to_string h in
      let h' = Id.from_string s in
      assert_equal ~printer:Id.to_string h h' );
    ( "to_path and from_path" >:: fun _ ->
      let h = Id.create "content" in
      let path = Id.to_path h in
      assert_equal ~printer:Fun.id "hash/9a/03/64/b9/e99bb480dd25e1f0284c8555"
        path;
      let h' = Id.from_path path in
      assert_equal ~printer:Id.to_string h h' );
  ]

let _ = run_test_tt_main ("Hash" >::: tests)
