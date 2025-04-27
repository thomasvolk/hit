
module Hash : sig
  type t = string

  val create : string -> t

  val to_path : t -> string
end
