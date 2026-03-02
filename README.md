# Ripe

A systems programming language.

## Installation

### Debian

```
sudo apt install opam
```

## References

- https://cs3110.github.io/textbook/chapters/interp/parsing.html
- https://mukulrathi.com/create-your-own-programming-language/parsing-ocamllex-menhir/
- https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html
- https://gallium.inria.fr/~fpottier/menhir/manual.html
- https://ocaml.org/manual/5.1/parsing.html
- https://ocaml.org/docs/cli-arguments

## Usage

```
dune build                            # Compile
dune exec ripe -- <file.rp>           # Run on a file
dune exec ripe -- examples/hello.rp   # Example
```

