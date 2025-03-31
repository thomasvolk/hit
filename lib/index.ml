
module Reference = struct
  type t = string * int list
end

module Document = struct
  type t = {
    path: string;
    source: string;
  }

  let ref t = Digest.MD5.to_hex (t.source ^ t.path)

  let create p s = {
    path = p;
    source = s
  }
end

module Entry = struct
  type t = string * Reference.t list
end

