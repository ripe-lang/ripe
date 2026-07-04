# Contributing

## Setup

1. Install [opam](https://opam.ocaml.org/doc/Install.html)
2. Run:

   ```sh
   opam switch create 5.3.0 ocaml.5.3.0
   opam install . --deps-only --yes
   dune build
   ```

## Running the Compiler

```sh
dune exec ripec -- <file.rp>
```

## Running Tests

```sh
dune test
```

## Adding Tests

Tests in `test/` use `let%expect_test` with an `[%expect {| |}]` block. Leave it
empty and let dune fill in the output:

```sh
dune test --auto-promote
```

Same command promotes new output after an intentional change.

## Formatting Code

```sh
dune fmt
```
