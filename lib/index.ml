open Sexplib.Std

module Query = struct
  type t =
    | Eq of string
    | Sw of string
    | Ew of string
    | Or of t list [@sexp.list]
    | And of t list [@sexp.list]
  [@@deriving sexp]

  let from_string s = t_of_sexp (Sexplib.Sexp.of_string s)

  module type QueryType = sig
    val from_string : string -> t
  end
end

type t = { path : string }

let create path = { path }

let add_doc t doc words =
  let fold_left f l t = List.fold_left f t l in
  let module TokenMap = Map.Make(Token.Id) in 
  let open Io in
  let doc_id = Doc.Id.create (Doc.path doc) in
  let token_doc_entry_path token_id =
    (t.path // Token.Id.to_path token_id // Doc.Id.to_string doc_id) ^ ".hit"
  in
  let tokens =
    Token.with_orders words
    |> List.map (fun e ->
        ( Token.Id.create (fst e),
          (Token.create (fst e), Token.DocumentEntry.create (snd e)) ))
  in
  let doc_dir = Doc.Id.to_path doc_id in
  let doc_tokens_file = t.path // doc_dir // "tokens.hit" in
  let current_doc_tokens =
    if file_exists doc_tokens_file then
      read_file_to_sexp doc_tokens_file |> Doc.TokenRefs.t_of_sexp
    else Doc.TokenRefs.empty
  in
  let token_map = TokenMap.of_list tokens in
  let tokens_to_delete = current_doc_tokens |> List.filter (fun e -> not (TokenMap.mem e token_map)) in
  let new_doc_tokens =
    List.map fst tokens
    |> List.fold_left (fun acc e -> Doc.TokenRefs.add e acc) Doc.TokenRefs.empty
  in
  let trx =
    Trx.empty
    |> fold_left
         (fun acc e -> Trx.add_delete_file (token_doc_entry_path e) acc)
         tokens_to_delete
    |> Trx.add_write_file doc_tokens_file
         (Doc.TokenRefs.sexp_of_t new_doc_tokens)
    |> Trx.add_write_file (t.path // doc_dir // "doc.hit") (Doc.sexp_of_t doc)
    |> fold_left
         (fun acc e ->
           Trx.add_write_file
             (token_doc_entry_path (fst e))
             (Token.DocumentEntry.sexp_of_t (snd (snd e)))
             acc
           |> Trx.add_write_file
                (t.path // Token.Id.to_path (fst e) // "token.hit")
                (fst (snd e) |> Token.sexp_of_t))
         tokens
  in
  execute_transaction
    (t.path // "trx" // (Doc.Id.to_string doc_id ^ ".hit"))
    trx

let add t path content =
  let tokens = Token.from_string path @ Token.from_string content
  and doc = Doc.create path (Doc.Checksum.create content) in
  add_doc t doc tokens
