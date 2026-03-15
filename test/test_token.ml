open OUnit2
open Hit

let tests = [
  ("from_string" >:: (fun _ ->
      let s = "\t1 \r 2   3 äöü,ß.foo \n 测试" in
      assert_bool "string is not valid utf8" (String.is_valid_utf_8 s);
      let t = Token.from_string s in
      assert_equal ["1"; "2"; "3"; "äöü"; "ß"; "foo"; "测试"] t
  ));
]

let _ = run_test_tt_main ("Token" >::: tests)
