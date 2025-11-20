module TokenMap : Map.S with type key = Text.Token.t

module DocumentTable : sig
  module DocumentTableId : sig
    val prefix : string
  end

  module Id : sig
    type t = string * string

    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
    val create : string -> t
    val prefix : t -> string
    val hash : t -> string
    val to_string : t -> string
    val of_string : string -> t
    val compare : t -> t -> int
  end

  type t = {
    id : Id.t;
    map :
      (Text.TokenEntry.Flags.t * Text.Token.Pos.t list) Document.DocumentMap.t;
  }

  val id : t -> Id.t
  val empty : Id.t -> t

  val add :
    Document.DocumentMap.key ->
    Text.TokenEntry.Flags.t * Text.Token.Pos.t list ->
    t ->
    t

  val get : Document.DocumentMap.key -> 'a Document.DocumentMap.t -> 'a option

  val all :
    t ->
    (Document.DocumentMap.key
    * (Text.TokenEntry.Flags.t * Text.Token.Pos.t list))
    list

  val filter :
    (Document.DocumentMap.key ->
    Text.TokenEntry.Flags.t * Text.Token.Pos.t list ->
    bool) ->
    t ->
    t

  val size : t -> int
  val merge : t -> t -> t
end

module DocumentTableMap : Map.S with type key = DocumentTable.Id.t

module TokenTable : sig
  type t = DocumentTable.Id.t TokenMap.t

  val add : TokenMap.key -> 'a -> 'a TokenMap.t -> 'a TokenMap.t
  val get : TokenMap.key -> 'a TokenMap.t -> 'a option

  val find_all :
    (TokenMap.key -> bool) -> 'a TokenMap.t -> (TokenMap.key * 'a) list

  val to_list : 'a TokenMap.t -> (TokenMap.key * 'a) list
  val empty : 'a TokenMap.t
  val size : 'a TokenMap.t -> int
  val merge : 'a TokenMap.t -> 'a TokenMap.t -> 'a TokenMap.t
end
