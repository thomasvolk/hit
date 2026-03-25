open Sexplib.Std

module Id = Hash.Make (struct
  let prefix = "tkn"
end)

type t = string [@@deriving sexp]

let create w = w

let from_string ?(token_start_char = 0x30) s =
  let split s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      let current = String.unsafe_get s i in
      if current < Char.chr token_start_char then (
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i)
    done;
    String.sub s 0 !j :: !r
  in
  split s |> List.filter (fun s -> String.length s > 0)

let with_orders tokens =
  let module StringMap = Map.Make (String) in
  tokens
  |> List.mapi (fun p t -> (t, p))
  |> List.rev
  |> List.fold_left
       (fun acc (t, p) ->
         let entry =
           match StringMap.find_opt t acc with
           | Some ol -> p :: ol
           | None -> [ p ]
         in
         StringMap.add t entry acc)
       StringMap.empty
  |> StringMap.to_list

module DocumentEntry = struct
  type t = int list [@@deriving sexp]
  let create orders = orders
end
