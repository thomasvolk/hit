
module type Persistence = sig
  type t
  type k
  type config

  val load : string -> config -> t

  val save : t -> config -> unit
end

module Make (P : Persistence) = struct
  type t = P.t
  type k = P.k
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

  type t = string

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
  type k = Index.Term.t

  type config = { 
    base_path : string;
  }

  let create path = { base_path = path }

  let index_path conf = Filename.concat conf.base_path "term-index"

  let entry_to_string e = 
    let open Index.TermIndex.Entry in
    let pl = positions e |> List.map string_of_int |> String.concat " "
    in
    ((Ref.to_string (ref e)) ^ " " ^ pl  |> String.trim)

  let term_index_to_string ti =
    let open Index.TermIndex in
    let rec build el s = match el with
      | [] -> s
      | (_, e) :: rest -> 
           build rest (s ^ (entry_to_string e ^ "\n"))
    in
    build (EntryMap.to_list ti.entries) ""

  let entry_of_string s =
    let open Index.TermIndex.Entry in
    let rl = String.trim s
      |> String.split_on_char ' '
      |> List.filter (fun t -> String.length t > 0)
    in
    match rl with
    | [_] | [] -> None
    | ref :: pl -> Some( create (Ref.of_string ref)  (List.map int_of_string pl))

  let load t conf = 
    let open Index.TermIndex in
    let ti = create t in
    let filename = Filename.concat (index_path conf) (Path.of_ref (ref ti)) in
    if Sys.file_exists filename then
      let rec add_rows t rl = match rl with
        | [] -> t
        | r :: rest -> 
            let tu = match entry_of_string r with
              | Some(r) -> add r t
              | None -> t
            in
            add_rows tu rest
      in
      add_rows ti (String.split_on_char '\n' (read_file filename))
    else
      ti

  let save ti conf =
    let filename = Filename.concat (index_path conf) (Path.of_ref (Index.TermIndex.ref ti)) in
    write_file (term_index_to_string ti) filename
end
