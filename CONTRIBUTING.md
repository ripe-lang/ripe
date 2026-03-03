# Contributing

> The compiler is in early stages. I'm not looking for new features.

## Setup

Install opam, then create a switch and install dependencies:

```
opam switch create 5.3.0 ocaml.5.3.0
opam install . --deps-only --yes
```

## Building

```
cd compiler
dune build
dune build @fmt
```

## Running

```
dune exec bin/main.exe -- -dump-ast <file.rp>
```

## Formatting

Make sure the code is formatted:

```
dune build @fmt --auto-promote
```
