open Sexplib.Std
open Text

type t = { config : Config.IndexConfig.t }

module QueryResult = struct
  type t = { doc_id : string; token_entries : Text.TokenEntry.t list }
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
  val get_doc : string -> t -> Document.t
  val get_doc_opt : string -> t -> Document.t option
  val get_entries : Token.t -> t -> (string * TokenEntry.t list) list

  val find_entries :
    (string -> bool) -> t -> (string * TokenEntry.t list) list
end

module type IndexType = sig
  include IndexReaderType

  val exists : unit -> bool
  val create : unit -> bool
  val load : unit -> t
  val add_doc : Document.t -> t -> t
  val delete_doc : string -> t -> t
  val token_count : t -> int
  val garbage_collect : t -> t
  val clear : t -> t
  val dump : t -> Core.Sexp.t
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
end

module Index = struct
end
