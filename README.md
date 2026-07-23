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

> [!WARNING]
> Ripe is in early development and is far from ready for real use. Expect breaking changes, missing features, and bugs.

## Installation

### Debian

```sh
sudo apt install opam qbe
opam init
opam install . --deps-only
```

## Usage

```
dune build
dune exec ripec -- <file.rp>
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

Ripe is licensed under GPL-2.0-only. See [COPYRIGHT.md](COPYRIGHT.md) for details.
