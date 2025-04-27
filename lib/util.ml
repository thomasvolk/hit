module Hash = struct
  type t = string
  
  exception InvalidHashInput of string

  let create = function 
    | "" -> raise (InvalidHashInput "can not hash an empty string")
    | s -> Digest.MD5.string s |> Digest.MD5.to_hex

  let folder_name_len = 2
  let folder_cnt = 4
  let hash_len = 32

  let to_path h =
    let rec add_path_sep p s = 
      let slen = String.length s in
      if slen <= (hash_len - (folder_cnt * folder_name_len))
      then p ^ s
      else
        let f = String.sub s 0 folder_name_len in
        let r = String.sub s folder_name_len (slen - folder_name_len) in
        add_path_sep (p ^ f ^ "/") r
    in
    add_path_sep "" h
end 

