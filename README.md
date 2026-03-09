<div align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/ripe-lang/www.ripe-lang.org/main/static/images/combination_light.png">
    <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/ripe-lang/www.ripe-lang.org/main/static/images/combination_dark.png">
    <img alt="Ripe: A systems programming language"
         src="https://raw.githubusercontent.com/ripe-lang/www.ripe-lang.org/main/static/images/combination_dark.png"
         width="50%">
  </picture>
</div>

A systems programming language.

## Installation

### Debian

```
sudo apt install opam
```

## Usage

```
dune build                            # Compile
dune exec ripec -- <file.rp>          # Run on a file
dune exec ripec -- examples/hello.rp  # Example
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

Ripe is licensed under GPL-2.0-only and MIT terms. See [COPYRIGHT.md](COPYRIGHT.md) for details.

## References

See [docs/references.md](docs/references.md).
