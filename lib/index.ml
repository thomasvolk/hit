open Sexplib.Std
open Io

module Query = struct
  type t =
    | Eq of string
    | Or of t list [@sexp.list]
    | And of t list [@sexp.list]
  [@@deriving sexp]

  let from_string s = t_of_sexp (Sexplib.Sexp.of_string s)
end

type t = { path : string }
type doc_files = { tokens_file : string; doc_file : string }

let fold_left f l t = List.fold_left f t l

let create path =
  let trx_path = path // Io.Trx.prefix in
  if Io.file_exists trx_path then
    Io.find_all_files
      ~predicate:(fun f ->
        String.starts_with ~prefix:Io.Trx.prefix (Filename.basename f))
      trx_path
    |> List.iter (fun f ->
        Io.read_file_to_sexp f |> Io.Trx.t_of_sexp |> Io.execute_actions;
        Io.delete_file f);
  { path }

let doc_files t doc_id =
  let doc_dir = t.path // Doc.Id.to_folder_path doc_id in
  { tokens_file = doc_dir // "tokens.hit"; doc_file = doc_dir // "doc.hit" }

let token_entry_file t doc_id token_id =
  (t.path // Token.Id.to_folder_path token_id // Doc.Id.to_string doc_id)
  ^ ".hit"

let execute t doc_id trx =
  execute_transaction
    (t.path // Io.Trx.prefix // (Doc.Id.to_string doc_id ^ ".hit"))
    trx

let doc_entries_of_token t token =
  let token_path =
    t.path // (Token.Id.create token |> Token.Id.to_folder_path)
  in
  Io.find_all_files
    ~predicate:(fun f ->
      String.starts_with ~prefix:Doc.id_prefix (Filename.basename f))
    token_path
  |> List.map (fun f ->
      ( Doc.Id.from_filename f,
        Io.read_file_to_sexp f |> Token.DocumentEntry.t_of_sexp ))

let get_doc t doc_id =
  let df = doc_files t doc_id in
  Io.read_file_to_sexp df.doc_file |> Doc.t_of_sexp

let query t q =
  let open Query in
  let score (d, l) = (d, Score.score l) in
  let merge ?(min_apperance = 0) doc_entries =
    let module DocIdMap = Map.Make (Doc.Id) in
    List.fold_left
      (fun acc (k, v) ->
        DocIdMap.update k
          (function Some cv -> Some (v @ cv) | None -> Some v)
          acc)
      DocIdMap.empty doc_entries
    |> DocIdMap.filter (fun _ e -> List.length e >= min_apperance)
    |> DocIdMap.to_list
  in
  let rec eval = function
    | Eq word ->
        let token = Token.UTF_8.lowercase word in
        doc_entries_of_token t token |> List.map (fun (d, e) -> (d, [ e ]))
    | Or ql -> List.map eval ql |> List.flatten |> merge
    | And ql ->
        List.map eval ql |> List.flatten
        |> merge ~min_apperance:(List.length ql)
  in
  eval (Query.from_string q)
  |> List.map score
  |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1)
  |> List.map fst

let add t ?(tokenizer = Token.from_string) path content =
  let doc = Doc.create path (Doc.Checksum.create content) in
  let doc_id = Doc.Id.create (Doc.path doc) in
  let df = doc_files t doc_id in
  let doc_is_up_to_date =
    if file_exists df.doc_file then
      let current_doc = read_file_to_sexp df.doc_file |> Doc.t_of_sexp in
      Doc.equal current_doc doc
    else false
  in
  if doc_is_up_to_date then doc_id
  else
    let words =
      tokenizer path @ tokenizer content |> List.map Token.UTF_8.lowercase
    in
    let module TokenMap = Map.Make (Token.Id) in
    let get_token_doc_entry_file = token_entry_file t doc_id in
    let tokens =
      Token.group words
      |> List.map (fun e ->
          ( Token.Id.create (fst e),
            (Token.create (fst e), Token.DocumentEntry.of_list (snd e)) ))
    in
    let current_doc_tokens =
      if file_exists df.tokens_file then
        read_file_to_sexp df.tokens_file |> Doc.TokenRefs.t_of_sexp
      else Doc.TokenRefs.empty
    in
    let token_map = TokenMap.of_list tokens in
    let tokens_to_delete =
      current_doc_tokens
      |> List.filter (fun e -> not (TokenMap.mem e token_map))
    in
    let new_doc_tokens =
      List.map fst tokens
      |> List.fold_left
           (fun acc e -> Doc.TokenRefs.add e acc)
           Doc.TokenRefs.empty
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
                (t.path // Token.Id.to_folder_path (fst e) // Token.file_name)
                (fst (snd e) |> Token.sexp_of_t))
         tokens
    |> execute t doc_id;
    doc_id

let delete t doc_id =
  let df = doc_files t doc_id in
  let doc_tokens =
    Io.read_file_to_sexp df.tokens_file |> Doc.TokenRefs.t_of_sexp
  in
  let get_token_doc_entry_file = token_entry_file t doc_id in
  Trx.empty
  |> fold_left
       (fun acc e -> Trx.add_delete_file (get_token_doc_entry_file e) acc)
       doc_tokens
  |> Trx.add_delete_file df.doc_file
  |> Trx.add_delete_file df.tokens_file
  |> execute t doc_id

let dump t =
  Io.find_all_files
    ~predicate:(fun f -> String.ends_with ~suffix:Token.file_name f)
    (t.path // Token.id_prefix)
  |> List.map (fun f ->
      let token = Token.t_of_sexp (Io.read_file_to_sexp f) in
      let base_path_len = String.length t.path in
      let token_path = Filename.dirname f in
      let token_id =
        Token.Id.from_folder_path
          (String.sub token_path (base_path_len + 1)
             (String.length token_path - base_path_len - 1))
      in
      let doc_entries =
        Io.find_all_files
          ~predicate:(fun f ->
            String.starts_with ~prefix:Doc.id_prefix (Filename.basename f))
          token_path
        |> List.map (fun f ->
            [
              Doc.Id.from_filename f |> Doc.Id.sexp_of_t; Io.read_file_to_sexp f;
            ]
            |> Core.Sexp.List)
      in
      Core.Sexp.List
        [
          Token.Id.sexp_of_t token_id;
          Token.sexp_of_t token;
          doc_entries |> Core.Sexp.List;
        ])
  |> Core.Sexp.List
