open OUnit2
open Hit

let tests =
  "Term" >::: [
    "add" >:: (
      fun _ ->
        let e = Term.empty
          |> Term.add (Term.Entry.create (Ref.create "foo") [1; 2; 3])
          |> Term.add (Term.Entry.create (Ref.create "bar") [4; 5; 6])
        in
        assert_equal ~printer:string_of_int 2 (Term.size e);
    );
    "of_string" >:: (
      fun _ ->
        let e = Term.of_string {|
        77dae9d8995deab0fe5e6ec3b2799a79 90 1   4  9
        00d2a735511a71b0d8449a57cf2520aa 1 7 90 66

        b97af9c9d2d47488f99c46ad69088129 0

      |} in 
      assert_equal ~printer:string_of_int 3 (Term.size e);
    );
    "to_string" >:: (
      fun _ ->
        let e = Term.empty
          |> Term.add (Term.Entry.create (Ref.create "a01") [1; 2; 3])
          |> Term.add (Term.Entry.create (Ref.create "a02") [4; 5; 6])
          |> Term.add (Term.Entry.create (Ref.create "a03") [6])
          |> Term.add (Term.Entry.create (Ref.create "a04") [56; 8; 9; 19])
        in
        let expected = {|00d2a735511a71b0d8449a57cf2520aa 1 2 3
77dae9d8995deab0fe5e6ec3b2799a79 56 8 9 19
b97af9c9d2d47488f99c46ad69088129 6
e3e810ec99978fecc00e9aa317b69aed 4 5 6
|} in
        assert_equal ~printer:Fun.id expected (Term.to_string e);
    );
    "invalid ref" >:: (
      fun _ ->
        assert_raises (Term.Entry.InvalidEntry "position list is empty") (fun () -> Term.Entry.create (Ref.create "123") []);
    )
  ]

let _ = 
  run_test_tt_main tests
