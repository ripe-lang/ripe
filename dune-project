(lang dune 3.21)

(name ripe)

(cram enable)

(generate_opam_files true)

(source
 (github ripe-lang/ripe))

(authors "Ripe contributors")

(maintainers "Ripe contributors")

(license "GPL-2.0-only")

(documentation https://github.com/ripe-lang/ripe#readme)

(package
 (name ripe)
 (synopsis "Ripe compiler and language tooling")
 (description
  "Ripe is a systems programming language with an OCaml compiler and QBE backend.")
 (depends ocaml ppx_deriving ocamlformat ppx_expect ppx_inline_test)
 (tags
  ("compiler" "language" "systems" "qbe")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
