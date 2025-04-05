

let read_file filename = 
    let ic = In_channel.open_text filename in
    try
      let content = In_channel.input_all ic in
      In_channel.close ic;
      content
    with exn ->
      In_channel.close ic;
      raise exn


let write_file filename content =
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc content;
  Out_channel.close oc
