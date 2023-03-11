(lang dune 3.6)

(name ocaml_exec_shell)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_exec_shell)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (>= 5.0.0)) dune eio_main)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
