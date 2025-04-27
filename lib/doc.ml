
type t = {
  path: string;
  origin: string;
}

let source t = t.origin ^ "::" ^ t.path

let name t = Filename.basename t.path

let create p o = {
  path = p;
  origin = o
}
