
module Reference : sig
  type t = string * int list
end

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

module Entry : sig
  type t = string * Reference.t list
end
