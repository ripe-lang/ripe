# Features

Reference for the language as it exists today.

## Expression-based language

```ripe
func main() i32 {
  let value = if true { 21 } else { 0 }
  value * 2
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

```ripe
let name: cstr = "ramon"     // immutable
var count: i32 = 0           // mutable
```

### Inferred bindings

```ripe
let x = 5        // type inferred from the value
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
extern func malloc(size: usize) *opaque
extern func free(ptr: *opaque)

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
func sum(xs: []i32) i32 { return xs[0] }
```

### Function pointers

```ripe
func f(x: i32) i32 { return x + 1 }

func apply(fn: (i32) i32, v: i32) i32 { return fn(v) }

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
extern func exit(code: i32) never

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

### if as a value

```ripe
func main() i32 {
  let big: i32 = if 3 > 2 { 10 } else { 20 }
  return big
}
```

### Block as a value

```ripe
func main() i32 {
  let x: i32 = {
    let a: i32 = 2
    a * 3
  }
  return x
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

```ripe
func main() i32 {
  var s: i32 = 0
  for i in 0..10 { s += i }
  return s
}
```

### Inclusive range

```ripe
func main() i32 {
  var s: i32 = 0
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

```ripe
func main() i32 { return sizeof(i32) as i32 }
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
extern func printf(fmt: cstr, ...) i32
extern func strlen(s: cstr) i64
```

### Integer literal suffixes

```ripe
func main() i32 {
  var a: u8 = 255u8
  var b: i64 = 9000000000i64
  return (a as i32) + (b as i32)
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
