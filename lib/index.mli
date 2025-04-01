type t = {
  path: string;
}

module Document : sig
  type t = private {
    path: string;
    origin: string;
  }
  val source : t -> string

  val name : t -> string

  val ref : t -> string

  val create : string -> string -> t
end

