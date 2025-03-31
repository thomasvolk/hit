
module Reference : sig
  type t = string * int list
end

module Document : sig
  type t = {
    ref: string;
    path: string;
    source: string;
  }
end

module Entry : sig
  type t = string * Reference.t list
end
