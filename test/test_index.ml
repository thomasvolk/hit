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
      let d = create (Meta.create "local" "docs/test01.txt") "my test document 01" in
      let _i' = Idx.add_doc d i in
      ()
    )
  ]

  
let _ = 
  run_test_tt_main tests

