
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


let write_file content filename =
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc content;
  Out_channel.close oc


