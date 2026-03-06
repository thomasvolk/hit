open Sexplib.Std
open Table
open Text
module DocumentMap = Document.DocumentMap

type t = {
  token_table : TokenTable.t;
  config : Config.IndexConfig.t;
}

module QueryResult = struct
  type t = { doc_id : Document.Id.t; token_entries : Text.TokenEntry.t list }
  [@@deriving sexp]

  let create d tel = { doc_id = d; token_entries = tel }
  let from_tuple (d, tel) = create d tel
  let doc_id sr = sr.doc_id
  let token_entries sr = sr.token_entries

  let closest_distances sr =
    let rec loop r c = function
      | [] -> r
      | n :: rest ->
          let r' =
            match Text.TokenEntry.closest_distance c n with
            | None -> r
            | Some d -> r @ [ d ]
          in
          loop r' n rest
    in
    let te = sr.token_entries |> List.filter Text.TokenEntry.has_positions in
    match te with [] -> [] | c :: rest -> loop [] c rest

  let score cfg sr =
    let count =
      List.map TokenEntry.count sr.token_entries
      |> List.mapi (fun i c -> c + (i * Config.IndexConfig.max_token_count cfg))
      |> List.fold_left ( * ) 1 |> Float.of_int
    and distances =
      List.map Float.of_int (closest_distances sr |> List.map TokenPair.distance)
      |> List.map (fun d -> 1. /. (1. +. d))
      |> List.fold_left ( +. ) 0.
    and meta =
      List.map TokenEntry.flags sr.token_entries
      |> List.fold_left
           (fun acc f ->
             if TokenEntry.Flags.title f then acc + 5
             else if TokenEntry.Flags.directory f then acc + 3
             else if TokenEntry.Flags.extension f then acc + 2
             else if TokenEntry.Flags.source f then acc + 1
             else acc)
           0
      |> Float.of_int
    in
    (1. +. count) *. (1. +. distances) *. (1. +. meta) |> Int.of_float

  let compare cfg a b = score cfg b - score cfg a
end

module type IndexReaderType = sig
  val get_doc : Document.Id.t -> t -> Document.t
  val get_doc_opt : Document.Id.t -> t -> Document.t option
  val get_entries : Token.t -> t -> (Document.Id.t * TokenEntry.t list) list

  val find_entries :
    (string -> bool) -> t -> (Document.Id.t * TokenEntry.t list) list
end

module type IndexType = sig
  include IndexReaderType

  val exists : unit -> bool
  val create : unit -> bool
  val load : unit -> t
  val add_doc : Document.t -> t -> t
  val delete_doc : Document.Id.t -> t -> t
  val token_count : t -> int
  val garbage_collect : t -> t
  val clear : t -> t
end

module Query = struct
  type idx_t = t

  type t =
    | Eq of string
    | Sw of string
    | Ew of string
    | Or of t list [@sexp.list]
    | And of t list [@sexp.list]
  [@@deriving sexp]

  let from_string s = t_of_sexp (Sexplib.Sexp.of_string s)

  module type QueryType = sig
    module Result = QueryResult
    val from_string : string -> t
    val query : t -> idx_t -> QueryResult.t list
    val find_docs : string list -> idx_t -> QueryResult.t list
  end

  module Make (Index : IndexReaderType) = struct
    module Result = QueryResult
    let from_string = from_string
    let rec and_filter m cnt = function
      | [] ->
          DocumentMap.filter (fun _ v -> cnt == List.length v) m
          |> DocumentMap.to_list
          |> List.map (fun (_, v) -> v)
      | result_list :: rest ->
          let rec process_list m = function
            | (did, tel) :: rest ->
                let m' =
                  match DocumentMap.find_opt did m with
                  | None -> DocumentMap.add did [ (did, tel) ] m
                  | Some l -> DocumentMap.add did ((did, tel) :: l) m
                in
                process_list m' rest
            | [] -> m
          in
          let m' = process_list m result_list in
          and_filter m' cnt rest

    let rec merge r = function
      | [] -> r
      | (did, el) :: tl ->
          let r' =
            match DocumentMap.find_opt did r with
            | None -> DocumentMap.add did el r
            | Some cel -> DocumentMap.add did (cel @ el) r
          in
          merge r' tl

    let or_op el =
      List.flatten el |> merge DocumentMap.empty |> DocumentMap.to_list

    let and_op el =
      el
      |> and_filter DocumentMap.empty (List.length el)
      |> List.flatten |> merge DocumentMap.empty |> DocumentMap.to_list

    let query q idx =
      let rec loop = function
        | Eq token -> Index.get_entries token idx
        | Sw s ->
            Index.find_entries (fun k -> String.starts_with ~prefix:s k) idx
        | Ew s -> Index.find_entries (fun k -> String.ends_with ~suffix:s k) idx
        | Or el -> List.map loop el |> or_op
        | And el -> List.map loop el |> and_op
      in
      loop q
      |> List.map QueryResult.from_tuple
      |> List.sort (QueryResult.compare idx.config)

    let find_docs tokens idx =
      Logs.info (fun m -> m "Search for tokens: %s" (String.concat " " tokens));
      List.map (fun t -> Index.get_entries t idx) tokens
      |> or_op
      |> List.map QueryResult.from_tuple
      |> List.sort (QueryResult.compare idx.config)
  end
end

module Make (Storage : Io.StorageInstance) = struct
  let load () =
    Logs.info (fun m -> m "Load index");
    {
      token_table = Storage.Impl.load_token_table Storage.t;
      config = Storage.Impl.load_index_config Storage.t;
    }

  let exists () = Storage.Impl.index_config_exists Storage.t

  let create () =
    if not (exists ()) then (
      let c = Config.IndexConfig.create () in
      Storage.Impl.save_index_config c Storage.t;
      true)
    else false

  let rec add_entries idx doc_id = function
    | [] -> 
        Storage.Impl.save_token_table idx.token_table Storage.t;
        idx
    | entry :: rest ->
        let token = TokenEntry.token entry in
        let dt_id = DocumentTable.Id.create token in
        let dt = Storage.Impl.load_doc_table dt_id Storage.t in
        let dt' =
          DocumentTable.add doc_id
            (TokenEntry.flags entry, TokenEntry.positions entry)
            dt
        in
        Storage.Impl.save_doc_table dt' Storage.t;
        let idx' =
          {
            idx with
            token_table = TokenTable.add token dt_id idx.token_table;
          }
        in
        add_entries idx' doc_id rest

  let get_doc did _idx = Storage.Impl.load_doc did Storage.t

  let get_doc_opt did _idx =
    if Storage.Impl.doc_exists did Storage.t then
      Some (Storage.Impl.load_doc did Storage.t)
    else None

  let update_doc d idx =
    let meta = Document.meta d and did = Document.id d in
    Logs.debug (fun m -> m "Parse document: %s" (Document.Meta.reference meta));
    let entries =
      Parser.parse
        (Config.IndexConfig.token_separators_seq idx.config)
        ~min_token_length:idx.config.min_token_length d
    in
    Logs.info (fun m ->
        m "Add document: %s - tokens found: %d"
          (Document.Meta.reference meta)
          (List.length entries));
    let idx' = add_entries idx did entries in
    Storage.Impl.save_doc d Storage.t;
    idx'

  let add_doc d idx =
    let meta = Document.meta d
    and did = Document.id d
    and csm = Document.checksum d in
    match Storage.Impl.load_doc_opt did Storage.t with
    | Some doc when Document.checksum doc = csm ->
        Logs.debug (fun m ->
            m "Skip document already indexed: %s" (Document.Meta.reference meta));
        idx
    | _ -> update_doc d idx

  let delete_doc did idx =
    match Storage.Impl.delete_doc did Storage.t with
    | false ->
        Logs.info (fun m ->
            m "Document not found: %s" (Document.Id.to_string did));
        idx
    | true -> idx

  let get_document_table_entries token dti =
    let dt = Storage.Impl.load_doc_table dti Storage.t in
    DocumentTable.all dt
    |> List.filter (fun (did, _) -> Storage.Impl.doc_exists did Storage.t)
    |> List.map (fun (did, (flags, pl)) ->
        (did, [ TokenEntry.create token pl flags ]))

  let get_entries token idx =
    match TokenTable.get token idx.token_table with | None -> []
    | Some dti -> get_document_table_entries token dti

  let find_entries predicate idx =
    TokenTable.find_all predicate idx.token_table
    |> List.map (fun (token, dti) -> get_document_table_entries token dti)
    |> List.flatten

  let token_count idx = TokenTable.size idx.token_table

  let garbage_collect idx =
    Logs.info (fun m -> m "Garbage collection start");
    (* Remove orphaned entries in the document tables *)
    let tokens =
      TokenTable.to_list idx.token_table
        |> List.filter
           (fun (_token, dt_id) ->
             let cleaned_document_table =
               Storage.Impl.load_doc_table dt_id Storage.t
               |> DocumentTable.filter (fun d_id _ ->
                   let exists = Storage.Impl.doc_exists d_id Storage.t in
                   if not exists then
                     Logs.info (fun m ->
                         m "DocumentTable[%s] remove orphaned document reference %s"
                           (DocumentTable.Id.to_string dt_id)
                           (Document.Id.to_string d_id));
                   exists)
              in
              Storage.Impl.save_doc_table cleaned_document_table Storage.t;
              DocumentTable.size cleaned_document_table > 0)
    in
    (* Remove orphaned entries in the token table *)
    let new_tt =
      tokens
      |> List.map fst
      |> List.fold_left
           (fun acc token ->
             Logs.info (fun m -> m "TokenTable remove orphaned token: %s" token);
             TokenTable.remove token acc) idx.token_table
    in
    let idx' = { idx with token_table = new_tt } in
    Storage.Impl.save_token_table idx'.token_table Storage.t;
    (* Remove orphaned documents *)
    let indexed_documents = tokens
               |> List.map snd
               |> List.map (fun dt_id -> Storage.Impl.load_doc_table dt_id Storage.t
               |> DocumentTable.all
               |> List.map fst)
               |> List.flatten
               |> List.sort_uniq String.compare
    in 
    let orphaned_documents =
      Storage.Impl.get_all_doc_ids Storage.t
               |> Io.DocumentIdSet.to_list
               |> List.filter (fun did -> (List.mem did indexed_documents) = false)
    in
    List.iter (fun did ->
                 Logs.info (fun m -> m "Remove orphaned document: %s" did);
                 ignore (Storage.Impl.delete_doc did Storage.t)
               ) orphaned_documents;
    Logs.info (fun m -> m "Garbage collection done");
    idx'

  let clear idx =
    let idx' =
      {
        idx with
        token_table = TokenTable.empty;
      }
    in
    Storage.Impl.save_token_table idx'.token_table Storage.t;
    garbage_collect idx'
end
