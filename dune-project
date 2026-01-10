(lang dune 3.17)

(name hit)

(generate_opam_files true)

(source
 (github thomasvolk/hit))

(authors "Thomas Volk <info@thomasvolk.de>")

(maintainers "Thomas Volk <info@thomasvolk.de>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hit)
 (synopsis "Full text search")
 (description "Hit is a library with a command line tool of a full text search implementation")
 (depends ocaml)
 (tags
  ("search" "full text")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
