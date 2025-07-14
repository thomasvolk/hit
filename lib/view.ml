
module Highlight = struct
  let lines d = 
    let rec loop r c pos = function
      | [] -> r
      | l :: tl -> 
          let r' = (c, pos, l) :: r in
          loop r' (c + 1) ((pos + (String.length l)) + 1) tl
    in
    loop [] 1 0 (String.split_on_char '\n' (Table.Document.content d))
end
