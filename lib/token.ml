let token_chars_start = 0x30


let from_string s =
  let split s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      let current = String.unsafe_get s i in
      if current < Char.chr token_chars_start then (
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i)
    done;
    String.sub s 0 !j :: !r
  in
  split s |> List.filter (fun s -> String.length s > 0)

