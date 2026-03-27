open Sexplib.Std
open Io

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
type doc_files = { tokens_file : string; doc_file : string }

let fold_left f l t = List.fold_left f t l

let create path = { path }

let doc_files t doc_id = 
  let doc_dir = t.path // Doc.Id.to_path doc_id in
  { tokens_file = doc_dir // "tokens.hit" ; doc_file = doc_dir // "doc.hit" }

let token_entry_file t doc_id token_id = 
  (t.path // Token.Id.to_path token_id // Doc.Id.to_string doc_id) ^ ".hit"

let execute t doc_id trx =
  execute_transaction
    (t.path // "trx" // (Doc.Id.to_string doc_id ^ ".hit"))
    trx

let add t ?(tokenizer=Token.from_string) path content =
  let words = tokenizer path @ tokenizer content
  and doc = Doc.create path (Doc.Checksum.create content) in
  let module TokenMap = Map.Make(Token.Id) in 
  let doc_id = Doc.Id.create (Doc.path doc) in
  let get_token_doc_entry_file = token_entry_file t doc_id
  in
  let tokens =
    Token.with_orders words
    |> List.map (fun e ->
        ( Token.Id.create (fst e),
          (Token.create (fst e), Token.DocumentEntry.create (snd e)) ))
  in
  let df = doc_files t doc_id in
  let current_doc_tokens =
    if file_exists df.tokens_file then
      read_file_to_sexp df.tokens_file |> Doc.TokenRefs.t_of_sexp
    else Doc.TokenRefs.empty
  in
  let token_map = TokenMap.of_list tokens in
  let tokens_to_delete = current_doc_tokens |> List.filter (fun e -> not (TokenMap.mem e token_map)) in
  let new_doc_tokens =
    List.map fst tokens
    |> List.fold_left (fun acc e -> Doc.TokenRefs.add e acc) Doc.TokenRefs.empty
  in
  Trx.empty
    |> fold_left
         (fun acc e -> Trx.add_delete_file (get_token_doc_entry_file e) acc)
         tokens_to_delete
    |> Trx.add_write_file df.tokens_file
         (Doc.TokenRefs.sexp_of_t new_doc_tokens)
    |> Trx.add_write_file df.doc_file (Doc.sexp_of_t doc)
    |> fold_left
         (fun acc e ->
           Trx.add_write_file
             (get_token_doc_entry_file (fst e))
             (Token.DocumentEntry.sexp_of_t (snd (snd e)))
             acc
           |> Trx.add_write_file
                (t.path // Token.Id.to_path (fst e) // "token.hit")
                (fst (snd e) |> Token.sexp_of_t))
         tokens
    |> execute t doc_id

let delete t doc_id =
  let df = doc_files t doc_id in
  let doc_tokens = Io.read_file_to_sexp df.tokens_file |> Doc.TokenRefs.t_of_sexp in
  let get_token_doc_entry_file = token_entry_file t doc_id in
  Trx.empty
    |> fold_left
         (fun acc e -> Trx.add_delete_file (get_token_doc_entry_file e) acc)
         doc_tokens
    |> Trx.add_delete_file df.doc_file
    |> Trx.add_delete_file df.tokens_file
    |> execute t doc_id

