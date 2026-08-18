# Contributing

## Setup

1. Install [opam](https://opam.ocaml.org/doc/Install.html)
2. Run:

   ```sh
   opam switch create 5.5.0 ocaml.5.5.0
   opam install . --deps-only --yes
   dune build
   ```

3. Enable the commit hook:

   ```sh
   git config core.hooksPath .githooks
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

Tests in `test/` use `let%expect_test` with an `[%expect {| |}]` block that dune
fills in when it starts empty:

```sh
dune test --auto-promote
```

The same command promotes new output after a real change.

`test/programs/` uses golden files instead of `%expect` blocks:

```
test/programs/<name>/
  main.rp           # input
  out.txt           # golden stdout
  compilererr.txt   # golden compile error
```

Promote a new golden file with:

```sh
test/programs/run.py --promote
```

## Formatting Code

```sh
dune fmt
```

See `docs/` for contributor guidelines and other project documentation.
