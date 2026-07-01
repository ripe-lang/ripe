# Contributing

> [!IMPORTANT]
> The compiler is in early stages. I'm not looking for new features.

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

## Formatting Code

```sh
dune fmt
```
