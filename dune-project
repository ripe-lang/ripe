(lang dune 3.21)

(name ripe)

(generate_opam_files true)

(source
 (github ripe-lang/ripe))

(authors "Ripe contributors")

(maintainers "Ripe contributors")

(license "Apache-2.0")

(documentation https://github.com/ripe-lang/ripe#readme)

(package
 (name ripe)
 (synopsis "Ripe compiler and language tooling")
 (description
  "Ripe is a systems programming language with an OCaml compiler and QBE backend.")
 (depends
  (ocaml
   (>= 5.5.0))
  climate
  ppx_deriving
  (ocamlformat :with-dev-setup)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test))
 (tags
  ("compiler" "language" "systems" "qbe")))
