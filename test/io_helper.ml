open Hit

let new_file_storage () =
  let test_path = "./test_storage_" ^ string_of_float (Unix.gettimeofday ()) in
  let module FileStorage = (val Io.file_storage test_path : Io.StorageInstance) in
  (module FileStorage : Io.StorageInstance)

let new_memory_storage () =
  let module InMemoryStorage = (val Io.in_memory_storage (200, 200) : Io.StorageInstance) in
  (module InMemoryStorage : Io.StorageInstance)
