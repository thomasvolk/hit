open OUnit2
open Hit

let test_docs =
  let open Document in
  [
    from_source "local" "docs/test01.txt" "1";
    from_source "local" "docs/test01.txt"
      "my test test document-01 01 foo bar x 1 x home";
    from_source "local" "docs/test02.txt"
      "my test test document-02 01 foo bar x 2 x bas town";
    from_source "local" "docs/test03.txt" "test 3 document-03";
    from_source "local" "docs/test04.txt" "document-04 hometown";
  ]

let test_path = "./test_index" ^ string_of_float (Unix.gettimeofday ())

module FileStorage = (val Io.file_storage test_path : Io.StorageInstance)

module InMemoryStorage =
  (val Io.in_memory_storage (200, 200) : Io.StorageInstance)

let tests (module Storage : Io.StorageInstance) =
  [
    ( "add and find" >:: fun _ ->
      let module Idx = Index.Make (Storage) in
      let module Q = Index.Query.Make (Idx) in
      ignore (Idx.create ());
      let idx = Idx.load () |> Idx.clear in
      let idx' = test_docs |> List.fold_left (fun i d -> Idx.add_doc d i) idx in
      assert_equal ~printer:Int.to_string 20 (Idx.token_count idx');
      let docs = Q.find_docs [ "foo"; "test" ] idx' in
      assert_equal ~printer:string_of_int 3 (List.length docs);
      let idx'' = Idx.garbage_collect idx in
      assert_equal ~printer:Int.to_string 20 (Idx.token_count idx'') );
    ( "QueryResult.distances" >:: fun _ ->
      let open Text.TokenEntry in
      let sr =
        Index.QueryResult.create (Document.Id.create "123")
          [
            create "t1" [ 1; 20; 89 ] Flags.empty;
            create "t2" [] Flags.empty;
            create "t3" [ 200; 430; 890 ] Flags.empty;
          ]
      in
      assert_equal
        ~printer:(fun l -> List.map string_of_int l |> String.concat " ")
        [ 111 ]
        (Index.QueryResult.closest_distances sr
        |> List.map Text.TokenPair.distance) );
    ( "QueryResult.score" >:: fun _ ->
      let open Text.TokenEntry in
      let cfg = Config.IndexConfig.create () in
      let sr =
        Index.QueryResult.create (Document.Id.create "123")
          [
            create "t1" [ 1; 20; 89 ] Flags.empty;
            create "t2" [ 6; 22; 400 ] Flags.empty;
            create "t3" [ 200; 430; 890 ] Flags.empty;
            create "t4" [] (Flags.empty |> Flags.set_title);
          ]
      in
      assert_equal [ 2; 30 ]
        (Index.QueryResult.closest_distances sr
        |> List.map Text.TokenPair.distance);
      assert_equal ~printer:Int.to_string 3982661845716783
        (Index.QueryResult.score cfg sr) );
    ( "add and query" >:: fun _ ->
      let module Idx = Index.Make (Storage) in
      let module Q = Index.Query.Make (Idx) in
      ignore (Idx.create ());
      let idx = Idx.load () |> Idx.clear in
      assert_equal ~printer:Int.to_string 0 (Idx.token_count idx);
      let idx' = test_docs |> List.fold_left (fun i d -> Idx.add_doc d i) idx in
      assert_equal ~printer:Int.to_string 20 (Idx.token_count idx');
      let result = Q.query (Index.Query.from_string "(sw home)") idx' in
      assert_equal ~printer:Int.to_string 2 (List.length result);
      let result = Q.query (Index.Query.from_string "(ew town)") idx' in
      assert_equal ~printer:Int.to_string 2 (List.length result);
      let result =
        Q.query (Index.Query.from_string "(or (eq foo) (eq bas))") idx'
      in
      assert_equal ~printer:Int.to_string 2 (List.length result);
      let result =
        Q.query (Index.Query.from_string "(and (eq foo) (eq bas))") idx'
      in
      assert_equal ~printer:Int.to_string 1 (List.length result);
      let result = Q.query (Index.Query.from_string "(eq foo)") idx' in
      assert_equal ~printer:Int.to_string 2 (List.length result) );
    ( "gc" >:: fun _ ->
      let module Idx = Index.Make (Storage) in
      let module Q = Index.Query.Make (Idx) in
      ignore (Idx.create ());
      let idx = Idx.load () |> Idx.clear in
      assert_equal ~printer:Int.to_string 0 (Idx.token_count idx);
      let doc = Document.from_source "local" "tcgest.txt" "first gc 003 400" in
      let idx = Idx.add_doc doc idx in
      assert_equal ~printer:Int.to_string 7 (Idx.token_count idx);
      let idx = Idx.delete_doc (Document.id doc) idx in
      let idx = Idx.garbage_collect idx in
      assert_equal ~printer:Int.to_string 0 (Idx.token_count idx);
    );
  ]

let _ =
  run_test_tt_main
    ("Index" >::: tests (module FileStorage) @ tests (module InMemoryStorage))
