
let read_file filename =
  let ic = In_channel.open_text filename in
  try
    let content = In_channel.input_all ic in
    In_channel.close ic;
    content
  with exn ->
    In_channel.close ic;
    raise exn

let rec create_dirs path =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir && Sys.is_directory dir) then (
    create_dirs dir;
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o755)

let write_file content filename =
  create_dirs filename;
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc content;
  Out_channel.close oc

let is_directory = Sys_unix.is_directory_exn ~follow_symlinks:false
let file_exists = Sys_unix.file_exists_exn ~follow_symlinks:true

let find_all_files ~predicate dir =
  let rec loop result = function
    (* by not following the symlinks we handle symlinks pointing to a dir as file *)
    | f :: tl when is_directory f ->
        Sys_unix.ls_dir f
        |> List.map (Filename.concat f)
        |> List.filter file_exists |> List.append tl |> loop result
    | f :: tl when predicate f -> loop (f :: result) tl
    | _ :: tl -> loop result tl
    | [] -> result
  in
  (* after we finished the loop we have to filter out all symbolic links pointing to directories *)
  loop [] (dir :: [])
  |> List.filter (fun f ->
      not (Sys_unix.is_directory_exn ~follow_symlinks:true f))
