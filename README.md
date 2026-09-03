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

> [!NOTE]
> I'm pausing new feature work for a while so I can build things with Ripe as it is. I want to figure out what the language needs instead of deciding every feature in advance. My main goals are that Ripe should compile quickly, stay low level, and feel easy to write, with the compiler handling routine bookkeeping while leaving control to the programmer. I'll still be doing a bunch of refactoring until then.

```go
import std.io

struct Point { x: i32; y: i32 }

func norm(p: Point) i32 {
  p.x * p.x + p.y * p.y
}

func main() i32 {
  var points: [2]Point = [
    Point { x: 1, y: 2 },
    Point { x: 8, y: 0 },
  ]
  for p in points {
    io.print_int(norm(p)) // 5 then 64
  }
  return 0
}
```

## Documentation

Read the documentation at [ripe-lang.org](https://www.ripe-lang.org).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

Ripe is licensed under Apache-2.0. See [COPYRIGHT.md](COPYRIGHT.md) for details.
