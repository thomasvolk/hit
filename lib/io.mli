(** Module for handling storage operations for documents, token tables, and
    document tables. *)

val read_file : string -> string
(** [read_file filename] reads the content of the file with the given [filename]
    and returns it as a string. *)

val hash_to_path : string -> string
(** [hash_to_path hash] converts a hash string into a file path by splitting it
    into folders. *)

val path_to_hash : string -> string
(** [path_to_hash path] converts a file path back into a hash string by removing
    the folder structure. *)

val find_all_files : predicate:(string -> bool) -> string -> string list
(** [find_all_files ~predicate path] recursively finds all files in the
    directory at [path] that satisfy the given [predicate]. *)

