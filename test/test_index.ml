open OUnit2
open Hit


let test_path = "./test_index/index"
module Storage = (val (Io.file_storage test_path) :  Io.StorageInstance)
module Idx = Index.Make(Storage)

let tests =
  "Index" >::: [
    "add" >:: (fun _ ->
      let open Model.Document in
      let i = Idx.create in
      let docs = 
        [
          (*ERROR: if a doc-table will be saved the second time the file format is broken *)
          create (Meta.create "local" "docs/test01.txt") "1";
          create (Meta.create "local" "docs/test01.txt") "1";
          (*
          create (Meta.create "local" "docs/test02.txt") "my test test document 02 01 foo bar x 2 x";
          create (Meta.create "local" "docs/test03.txt") "test 3";
          *)
        ] in
      let _i' = docs |> List.fold_left (fun i d -> Idx.add_doc d i) i in
      ()
    )
  ]

  
let _ = 
  run_test_tt_main tests

