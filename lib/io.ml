open Sexplib.Std

module Action = struct
  type t = WriteFile of string * Core.Sexp.t | DeleteFile of string [@@deriving sexp]
  let of_write_file p s = WriteFile (p, s) 
  let of_delete_file p = DeleteFile p
end

module Trx = struct
  type t = Action.t list [@@deriving sexp]
  let empty = []
  let add a t = List.append [a] t
end

let read_file path =
  let ic = In_channel.open_text path in
  try
    let content = In_channel.input_all ic in
    In_channel.close ic;
    content
  with exn ->
    In_channel.close ic;
    raise exn

let read_file_to_sexp path =
  Core.Sexp.of_string (read_file path)

let rec create_dirs path =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir && Sys.is_directory dir) then (
    create_dirs dir;
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o755)

let write_file path content =
  create_dirs path;
  let oc = Out_channel.open_text path in
  Out_channel.output_string oc content;
  Out_channel.close oc

let write_file_from_sexp path sexp = write_file path (Core.Sexp.to_string sexp)

let file_exists = Sys.file_exists

let delete_file path =
  if file_exists path then Sys.remove path

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

let delete_all_files ~predicate dir =
  List.iter delete_file (find_all_files ~predicate dir)

exception TransactionError of string

let execute_transaction ?(check_for_existing=true) path tx =
  if file_exists path && check_for_existing then raise (TransactionError (Format.sprintf "transaction already exists: %s" path));
  write_file_from_sexp path (Trx.sexp_of_t tx);
  let open Action in
  List.iter (function 
      | WriteFile (p, c) -> write_file_from_sexp p c
      | DeleteFile p -> delete_file p)
    tx;
  delete_file path
