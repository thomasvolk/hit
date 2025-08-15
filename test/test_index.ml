open OUnit2
open Hit

let test_path = "./test_index/index"

module Storage = (val Io.file_storage test_path : Io.StorageInstance)
module Idx = Index.Make (Storage)

let tests =
  "Index"
  >::: [
         ( "add and find" >:: fun _ ->
           let open Table.Document in
           Idx.init ();
           let idx = Idx.load () in
           let docs =
             [
               create (Meta.create "local" "docs/test01.txt") "1";
               create
                 (Meta.create "local" "docs/test01.txt")
                 "my test test document-01 01 foo bar x 1 x";
               create
                 (Meta.create "local" "docs/test02.txt")
                 "my test test document-02 01 foo bar x 2 x";
               create
                 (Meta.create "local" "docs/test03.txt")
                 "test 3 document-03";
               create (Meta.create "local" "docs/test04.txt") "document-04";
             ]
           in
           let idx' = docs |> List.fold_left (fun i d -> Idx.add_doc d i) idx in
           let docs = Idx.find_docs [ "foo"; "test" ] idx' in
           assert_equal ~printer:string_of_int 3 (List.length docs) );
         ( "SearchResult.distances" >:: fun _ ->
           let sr =
             Index.SearchResult.create
               (Table.Document.Id.create "123")
               [
                 Text.TokenEntry.create "t1" [ 1; 20; 89 ];
                 Text.TokenEntry.create "t2" [];
                 Text.TokenEntry.create "t3" [ 200; 430; 890 ];
               ]
           in
           assert_equal
             ~printer:(fun l -> List.map string_of_int l |> String.concat " ")
             [ 111 ]
             (Index.SearchResult.best_matches sr
             |> List.map Text.TokenPair.distance) );
         ( "SearchResult.score" >:: fun _ ->
           let cfg = Config.IndexConfig.create () in
           let sr =
             Index.SearchResult.create
               (Table.Document.Id.create "123")
               [
                 Text.TokenEntry.create "t1" [ 1; 20; 89 ];
                 Text.TokenEntry.create "t2" [ 6; 22; 400 ];
                 Text.TokenEntry.create "t3" [ 200; 430; 890 ];
               ]
           in
           assert_equal [ 2; 30 ]
             (Index.SearchResult.best_matches sr
             |> List.map Text.TokenPair.distance);
           assert_equal ~printer:Int.to_string 7375299715
             (Index.SearchResult.score cfg sr) );
       ]

let _ = run_test_tt_main tests
