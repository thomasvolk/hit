module TestMap = Map.Make(Int)

let () =
  let m = TestMap.empty in
  let rec fill m = function
    | n when n < 0 -> m
    | n -> fill (TestMap.add n ("term", [0;1;2;3;4;5;6;7;8;9]) m) (n - 1)
  in
  let m = fill m (10 * 1000 * 1000) in
  print_endline ("Map created with " ^ (string_of_int (TestMap.cardinal m) ^ " entries - now sleep"));
  Unix.sleep 240;
