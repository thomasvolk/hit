
module type Persistence = sig
  type t
  type config

  val load : string -> config -> t

  val save : string -> t -> config -> unit
end

module Make (P : Persistence) = struct
  type t = P.t
  type config = P.config
  
  let load n c = P.load n c

  let save t c = P.save t c
end

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
  if not (Sys.file_exists dir && Sys.is_directory dir) then begin
    create_dirs dir;
    Sys.mkdir dir 0o755;
  end

let write_file content filename =
  create_dirs filename;
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc content;
  Out_channel.close oc


module Path = struct

  let of_ref r =
    let folder_name_len = 2 in
    let folder_cnt = 4 in
    let hash_len = 32 in
    let rec add_path_sep p s = 
      let slen = String.length s in
      if slen <= (hash_len - (folder_cnt * folder_name_len))
      then p ^ s
      else
        let f = String.sub s 0 folder_name_len in
        let r = String.sub s folder_name_len (slen - folder_name_len) in
        add_path_sep (p ^ f ^ "/") r
    in
    add_path_sep "" (Ref.to_string r)

  let to_string t = t

end


module TermIndexFile = struct
  type t = Index.TermIndex.t

  type config = { 
    base_path : string;
  }

  let create path = { base_path = path }

  let index_path c = Filename.concat c.base_path "index"

  let load w c = 
    let open Util in
    let filename = Filename.concat (index_path c) (Hash.create w |> Hash.to_path) in
    if Sys.file_exists filename then
      Term.of_string (read_file filename)
    else
      Term.empty 

  let save r c =
    let open Util in
    let filename = Filename.concat (index_path c) (Path.of_ref (Index.TermIndex.ref r)) in
    write_file (Term.to_string r) filename
end
