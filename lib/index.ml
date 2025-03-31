
module Reference = struct
  type t = string * int list
end

module Document = struct
  type t = {
    ref: string;
    path: string;
    source: string;
  }
end

module Entry = struct
  type t = string * Reference.t list
end

