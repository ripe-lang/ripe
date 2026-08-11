# Features

Reference for the language as it exists today.

## Expression-based language

An `if`, a block, and a `loop` are values anywhere a value is expected.

```ripe
func twice(x: i32) i32 { x * 2 }

func main() i32 {
  let value = if true { 21 } else { 0 }
  twice({ value })
}
```

## Comments

```ripe
// line comment

/* block comment
   /* nested */
*/
```

## Declarations and bindings

### let and var

A binding without an annotation takes its type from the value.

```ripe
let name: cstr = "ramon"     // immutable
var count: i32 = 0           // mutable
let x = 5                    // inferred
var total = 0
```

### comptime

A comptime is a value the compiler works out ahead of time. For now it has to be a scalar like an int or a float or a bool. Use let when you need storage.

```ripe
comptime N: i32 = 3

func main() i32 {
  var a: [N]i32 = undefined
  return a.len as i32
}
```

### Zero init and undefined

```ripe
var counter: i32              // zero init
var buf: [64]u8 = undefined   // skip zero init
```

## Types

### Primitive types

```ripe
var a: i8
var b: i16
var c: i32
var d: i64
var e: u8
var f: u16
var g: u32
var h: u64
var i: usize
var j: isize
var k: f32
var l: f64
var m: bool
var n: cstr
var o: char
```

### Short names

`int` and `float` are other spellings for `i64` and `f64`.

```ripe
var a: int = 5
var b: float = 1.5
```

### Untyped number literals

A literal takes its type from where it lands. When there's no context an integer is `i32` and a float is `f64`.

```ripe
func take(x: u8) i32 { return x as i32 }

func main() i32 {
  var a: u8 = 255
  var b: f32 = 1
  var c: u8 = 200
  var d = c + 1
  var e: f32 = 1.5
  var f = e + 1
  return take(1) + (d as i32) + (f as i32)
}
```

An integer literal is exact so a type that can't hold it errors. A float literal rounds.

```ripe
var a: u8 = 300           // error: does not fit in u8
var b: f32 = 16777217     // error: becomes 16777216
var c: f32 = 16777217.0   // fine: a float literal rounds
var d: f32 = 0.1          // fine: no binary float holds this exactly
```

### Integer literal suffixes

```ripe
func main() i32 {
  var a: u8 = 255u8
  var b: i64 = 9000000000i64
  return (a as i32) + (b as i32)
}
```

### Float literal suffixes

```ripe
func main() i32 {
  var a: f32 = 1.5f32
  var b: f64 = 2.5f64
  var c = 1f32
  var d = 1e3f64
  return (a as i32) + (b as i32) + (c as i32) + (d as i32)
}
```

### Hex, binary, and octal literals

```ripe
func main() i32 {
  var a: i32 = 0xff
  var b: i32 = 0b1010
  var c: i32 = 0o17i32
  return a + b + c
}
```

### Digit separators

An underscore after the first digit is ignored. One right after a base prefix is rejected.

```ripe
func main() i32 {
  var a: int = 1_000_000
  var b: i32 = 0xff_ff
  var c: f64 = 1_000.000_1
  // var d: i32 = 0x_ff        // rejected
  return (a as i32) + b + (c as i32)
}
```

### Float literals

```ripe
func main() i32 {
  var a: f64 = 1.5
  var b: f64 = 1e10
  return (a + b) as i32
}
```

### String literals

```ripe
func main() i32 {
  var s: cstr = "line one\nline two\tend\\\"quoted\""
  return 0
}
```

### str

A `str` is a `{ptr, len}` pair pointing at bytes that are always valid UTF 8.

```ripe
func take(s: str) usize {
  return s.len
}

func main() i32 {
  let s: str = "héllo"   // 5 characters, 6 bytes
  return take(s) as i32  // 6
}
```

### Char literals

```ripe
func main() i32 {
  var a: char = 'a'
  var nl: char = '\n'
  var tab: char = '\t'
  var bs: char = '\\'
  var q: char = '\''
  var z: char = '\0'
  return 0
}
```

### Struct declarations and literals

```ripe
struct point { x: i32, y: i32 }

func main() i32 {
  var p: point = point { x: 3, y: 4 }
  return p.x
}
```

### Type alias

```ripe
type byte = u8
```

### Newtype

```ripe
newtype Celsius = f32
```

### Local declarations

```ripe
func distance() i32 {
  struct Point { x: i32, y: i32 }
  type Coord = i32

  func add(left: Coord, right: Coord) Coord {
    left + right
  }

  let point = Point { x: 3, y: 4 }
  add(point.x, point.y)
}
```

### Pointers

```ripe
func main() i32 {
  var x: i32 = 10
  var q: *i32 = &x
  *q = 42
  return x
}
```

### Opaque pointers

```ripe
extern "C" func malloc(size: usize) *opaque
extern "C" func free(ptr: *opaque)

func main() i32 {
  var p: *opaque = malloc(64)
  free(p)
  return 0
}
```

### Fixed arrays

```ripe
func main() i32 {
  var a: [3]i32 = [4, 5, 6]
  return a[0]
}
```

### Multi dimensional arrays

```ripe
func main() i32 {
  var m: [2][2]i32 = [[1, 2], [3, 4]]
  m[1][0] = 7
  return m[1][0]
}
```

### Array length

```ripe
func main() i32 {
  var a: [3]i32 = [4, 5, 6]
  var s: i32 = 0
  for i in 0..a.len { s += a[i] }
  return s
}
```

### Slices

```ripe
func sum(xs: []i32) i32 {
  var t: i32 = 0
  for i in 0..xs.len { t += xs[i] }
  return t
}

func main() i32 {
  var a: [3]i32 = [4, 5, 6]
  return sum(a[..])
}
```

An array won't turn into a slice on its own. You write the range so you can see
what you're getting. `var b: [3]i32 = a` copies the whole thing while
`var s: []i32 = a[..]` just points at it.

### Slicing

Leave off the front and it starts at 0. Leave off the back and it runs to the
end. The back is always exclusive unless you use `..=`.

```ripe
func main() i32 {
  var a: [4]i32 = [10, 20, 30, 40]
  var whole: []i32 = a[..]      // all 4
  var to: []i32 = a[..2]        // 10 and 20
  var upto: []i32 = a[..=2]     // 10, 20 and 30
  var from: []i32 = a[2..]      // 30 and 40
  var s: []i32 = a[1..3]        // 20 and 30
  var t: []i32 = a[1..=2]       // 20 and 30, the 2 is included
  return (whole.len + to.len + upto.len + from.len + s.len + t.len) as i32
}
```

### Slice length and data pointer

```ripe
func main() i32 {
  var a: [3]i32 = [4, 5, 6]
  var s: []i32 = a[0..2]
  var p: *i32 = s.ptr
  return *p
}
```

### Function pointers

```ripe
func f(x: i32) i32 { return x + 1 }

func apply(fn: func (i32) i32, v: i32) i32 { return fn(v) }

func apply_c(fn: extern "C" func (i32) i32, v: i32) i32 { return fn(v) }

func main() i32 { return apply(f, 4) }
```

## Functions

### Function definitions

```ripe
func add(a: i32, b: i32) i32 { return a + b }
```

### Implicit return

```ripe
func square(x: i32) i32 { x * x }

func abs(x: i32) i32 {
  if x < 0 { -x } else { x }
}
```

### Bare return

```ripe
func main() i32 {
  if true { return }
  return 1
}
```

### Never return type

```ripe
extern "C" func exit(code: i32) never

func main() i32 {
  exit(0)
}
```

## Globals

```ripe
var counter: i32 = 5

func bump() { counter += 3 }

func main() i32 {
  bump()
  return counter
}
```

## Control flow

### if, else if, else

```ripe
func classify(x: i32) i32 {
  if x < 0 {
    return 0
  } else if x == 0 {
    return 1
  } else {
    return 2
  }
}
```

### while

```ripe
func main() i32 {
  var i: i32 = 0
  while i < 7 { i += 1 }
  return i
}
```

### for over a range

The end is exclusive unless you write `..=`.

```ripe
func main() i32 {
  var s: i32 = 0
  for i in 0..10 { s += i }
  for x in 0..=4 { s += x }
  return s
}
```

### break and continue

```ripe
func main() i32 {
  var s: i32 = 0
  for i in 0..10 {
    if i == 3 { continue }
    if i == 6 { break }
    s += i
  }
  return s
}
```

### Loop labels

```ripe
func main() i32 {
  var s: i32 = 0
  outer: for i in 0..4 {
    for j in 0..4 {
      if j == 2 { continue :outer }
      if i == 3 { break :outer }
      s += i * j
    }
  }
  return s
}
```

### loop

```ripe
func main() i32 {
  var n: i32 = 0
  loop {
    n += 1
    if n == 5 { break }
  }
  return n
}
```

### Loop values

```ripe
func main() i32 {
  var n: i32 = 0
  let answer: i32 = loop {
    n += 1
    if n == 7 { break n * 6 }
  }
  return answer
}
```

## Operators

### Arithmetic and comparison

```ripe
func main() i32 { return (1 + 2) * 3 - 4 / 2 % 3 }
```

### Bitwise and shifts

```ripe
func main() i32 {
  return (12 & 10) | (1 ^ 3) | (~5 & 255) | (1 << 4) | (256 >> 2)
}
```

### Logical

```ripe
func check(a: bool, b: bool) bool { return a && b || !a }
```

### Compound assignment

```ripe
func main() i32 {
  var x: i32 = 1
  x += 2
  x -= 1
  x *= 3
  x /= 2
  return x
}
```

### Swap

```ripe
func main() i32 {
  var a: i32 = 1
  var b: i32 = 2
  a, b = b, a
  return a
}
```

### Cast

```ripe
func main() i32 {
  var f: f64 = 2.5
  return f as i32
}
```

### sizeof

A size takes its type from where it lands. With no context it's a `usize`.

```ripe
extern "C" func malloc(n: u64) *opaque

struct pt { x: i32, y: i32 }

func main() i32 {
  let a: u64 = sizeof(pt)
  let b = sizeof(pt)          // usize
  let p = malloc(sizeof(pt))  // no cast needed
  return sizeof(i32)
}
```

## Modules

### Imports

```ripe
import math.vector

func main() i32 {
  return vector.first()
}
```

An import binds the last part of the path so `math.vector` is written `vector`.

### Public visibility

A declaration is private to its module unless it opens with `pub`.

```ripe
pub func add(left: i32, right: i32) i32 {
  return left + right
}

pub struct point { x: i32, y: i32 }

pub comptime WIDTH: i32 = 6
pub let SCALE: i32 = 7
pub var TOTAL: i32 = 0
pub type Count = i32
pub newtype Celsius = i32
```

A `pub var` is one storage location that every importing module reads and
writes, so `pub` says who can see it and `var` says whether it can change.

An `extern` is always private, so each module declares the foreign symbols it
calls.

### Directory modules

Files in a directory that open with the same `module` clause share one namespace
and one private scope.

```ripe
// shapes/area.rp
module shapes

pub func area() i32 {
  return width() * height()
}
```

```ripe
// shapes/sides.rp
module shapes

func width() i32 {
  return 6
}

func height() i32 {
  return 7
}
```

```ripe
// main.rp
import shapes

func main() i32 {
  return shapes.area()
}
```

A file without a clause is its own module named by its path. A directory whose
files carry no clause stays a plain namespace step.

## FFI

### Foreign functions

```ripe
extern "C" func printf(fmt: cstr, ...) i32
extern "C" func strlen(s: cstr) i64
```
