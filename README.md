# Ripe

A systems programming language.

## Installation

### Debian

```
sudo apt install opam
```

## Usage

```
dune build                            # Compile
dune exec ripe -- <file.rp>           # Run on a file
dune exec ripe -- examples/hello.rp   # Example
```

## TODO

- [ ] lexer
- [ ] parser
- [ ] AST
- [ ] type checker
- [ ] code generator, emits QBE IR
