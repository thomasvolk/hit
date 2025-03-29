
let r_lf = [
  Str.regexp "\r\n"; (* Windows *)
  Str.regexp "\n\r"; (* RISC OS *)
  Str.regexp "\r"    (* Apple II, Commodore *)
]

let posix_lf = "\n"

let normalize_linefeed s =
  let rec normalize rl s = match rl with
    | r :: l -> normalize l (Str.global_replace r posix_lf s)
    | [] -> s
  in
  normalize r_lf s
  
