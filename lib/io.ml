

let read_file filename = 
  if Sys.file_exists filename then
    let ic = In_channel.open_text filename in
    try
      let content = In_channel.input_all ic in
      In_channel.close ic;
      Some content
    with exn ->
      In_channel.close ic;
      raise exn
  else
    None
