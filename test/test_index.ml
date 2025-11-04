open OUnit2
open Hit

let test_path = "./test_index/index"

module Storage = (val Io.file_storage test_path : Io.StorageInstance)
module Idx = Index.Make (Storage)
module Q = Index.Query.Make (Idx)

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

let tests =
  "Index"
  >::: [
         ( "add and find" >:: fun _ ->
           Idx.init ();
           let idx = Idx.load () in
           let idx' =
             test_docs |> List.fold_left (fun i d -> Idx.add_doc d i) idx
           in
           let docs = Q.find_docs [ "foo"; "test" ] idx' in
           assert_equal ~printer:string_of_int 3 (List.length docs) );
         ( "SearchResult.distances" >:: fun _ ->
           let sr =
             Index.SearchResult.create (Document.Id.create "123")
               [
                 Text.TokenEntry.create "t1" [ 1; 20; 89 ];
                 Text.TokenEntry.create "t2" [];
                 Text.TokenEntry.create "t3" [ 200; 430; 890 ];
               ]
           in
           assert_equal
             ~printer:(fun l -> List.map string_of_int l |> String.concat " ")
             [ 111 ]
             (Index.SearchResult.closest_distances sr
             |> List.map Text.TokenPair.distance) );
         ( "SearchResult.score" >:: fun _ ->
           let cfg = Config.IndexConfig.create () in
           let sr =
             Index.SearchResult.create (Document.Id.create "123")
               [
                 Text.TokenEntry.create "t1" [ 1; 20; 89 ];
                 Text.TokenEntry.create "t2" [ 6; 22; 400 ];
                 Text.TokenEntry.create "t3" [ 200; 430; 890 ];
               ]
           in
           assert_equal [ 2; 30 ]
             (Index.SearchResult.closest_distances sr
             |> List.map Text.TokenPair.distance);
           assert_equal ~printer:Int.to_string 7375299715
             (Index.SearchResult.score cfg sr) );
         ( "add and query" >:: fun _ ->
           Idx.init ();
           let idx = Idx.load () in
           let idx' =
             test_docs |> List.fold_left (fun i d -> Idx.add_doc d i) idx
           in
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
       ]

let _ = run_test_tt_main tests
