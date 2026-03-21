open OUnit2
open Hit

let tests = [
  ("from_string" >:: (fun _ ->
    let s = "\t1 \r 2   3 äöü,ß.foo \n 测试" in
    assert_bool "string is not valid utf8" (String.is_valid_utf_8 s);
    let t = Token.from_string s in
    assert_equal ["1"; "2"; "3"; "äöü"; "ß"; "foo"; "测试"] t
  ));
  ("with_orders" >:: (fun _ ->
    let with_orders s = Token.from_string s |> Token.with_orders in
    let s = "/0/1/2/3/4.txt 1 \t2 \r foo  bar äöü,ß.foo test test \n 测试 foo äöü test end" in
    assert_bool "string is not valid utf8" (String.is_valid_utf_8 s);
    assert_equal 0 (List.length (with_orders ""));
    assert_equal 1 (List.length (with_orders "one"));
    assert_equal 1 (List.length (with_orders "one one"));
    assert_equal 2 (List.length (with_orders "one one two"));
    let tl = with_orders s in
    assert_equal ~printer:string_of_int 13 (List.length tl);
    assert_equal ("0", [0]) (List.nth tl 0);
    assert_equal ("1", [1; 6]) (List.nth tl 1);
  ));
]

let _ = run_test_tt_main ("Token" >::: tests)
