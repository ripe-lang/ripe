# Ripe Programming Language

This is a basic outline of the language to get an MVP going while I develop the compiler.

## Primitives

```r
bool                            // true, false
i8, i16, i32, i64, i128, isize  // signed integers
u8, u16, u32, u64, u128, usize  // unsigned integers
f32, f64                        // floats
```

## Variables

```r
var x = 10
var y: i32 = 20    // explicit type
```

## Expressions

```r
// Arithmetic: + - * / %
// Comparison: == != < <= > >=
// Logical: ! && ||
// Bitwise: & | ^ ~ << >>
```

## Functions

```r
func add(a: i32, b: i32): i32 {
    a + b
}
```

## Control Flow

```r
// Conditionals
if condition { } else { }

// Loops
while condition { }
for var i = 0; i < 10; i++ { }
```
