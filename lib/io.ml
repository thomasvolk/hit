
module type Operator = sig
  type t
  type config

  val load : string -> config -> t

  val save : string -> t -> config -> unit
end

module Make (O : Operator) = struct
  type t = O.t
  type config = O.config
  
  let load n c = O.load n c

  let save n t c = O.save n t c
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

module FileIndex = struct
  type t = Index.t

  type config = { 
    base_path : string;
  }

  let create path = { base_path = path }

  let index_path c = Filename.concat c.base_path "index"

  let load w c = 
    let open Util in
    let filename = Filename.concat (index_path c) (Hash.create w |> Hash.to_path) in
    if Sys.file_exists filename then
      Index.of_string (read_file filename)
    else
      Index.empty 

  let save w r c =
    let open Util in
    let filename = Filename.concat (index_path c) (Hash.create w |> Hash.to_path) in
    write_file (Index.to_string r) filename
end
