# Diagnostics

A design document of how to use `diagnostic.ml`.

## Template

```text
error: <rule that broke>
  at <file>:<line>:<col>
    <source line>
    ^~~~ <the specific values>
help: <what to do about it>
```

You build this using the pipeline (never by hand).

```ocaml
error "type mismatch" |> at span |> label "expected i32, found void"
```

## Rules

### 1. How to write a message

I write my messages in lowercase, no trailing period, and a sentence fragment.

**Headline** is what names the rule, so no types, no names, and no counts.

```text
type mismatch
already defined
invalid operand
```

**Primary label** carries the values.

```text
cannot apply `+` to bool
expected `;`
cannot zero init this type
```

**Secondary** is the other place that matters

```text
previous definition here
unclosed `{`
declared private here
```

**Suggestion** is the fix (real syntax)

```text
prefix with an underscore: _p
move the operator to the previous line
use `var _ = ...` when this is intentional
```

Most of the time you'll be using:

```text
expected <a>, found <b>
on <type>                   the type the operation was tried on
```

There are two more fields but they're only used in special cases:

* **Note**: sub diagnostics that's used when the second thing needs its own snippet
* **Detail**: printed as-is that's used by internal compiler error

The main goal is to be direct, stating what is true not what the
compiler did.

### 2. Drop what the source already shows

```text
error: undefined variable
  at t.rp:1:8
    return foo
           ^~~
```

The `foo` is already under the caret, so naming it again adds nothing. The only exception is when the value is not on the line, such as an inferred type:

```text
error: type has no fields
  at t.rp:4:13
      var _y = *n.x + 1
                ^~~ on i32
```

### 3. Spans

The point is to be obvious and minimal about where the fault is at.

```text
error: no field
    return p.zebra
             ^~~~~ on struct pt
```

You should point to the smallest span that can identify the failure, not the whole thing:

```text
error: no field
    return p.zebra
           ^~~~~~~ on struct pt
```

The AST carries a name span for this, such as `func_name_span`, `struct_name_span`, etc.

```text
error: already defined
  at t.rp:6:6
    func wide(a: i32, b: i32) i32 {
         ^~~~
```

There might be where the caret that is shown is a few dozen columns. This is just the result of whatever node was in hand.

### 4. Severity

| type | desc |
| ---- | ---- |
| error | the program is rejected |
| warning | the program compiles and something is probably wrong |
| note | context belongs to another diagnostic |
| help | closing suggestion |

Known issue: I haven't fully fleshed out the warning system. I still don't have a way to opt out of it.

### 5. Avoid cascading

This is not about grouping all unused variables into one message: but rather one cause, one diagnostic.

```text
error: module not found: math
  at main.rp:1:1
    import math
    ^~~~~~~~~~~

(and not a second error for math.answer that was not found)
```

### 6. Long lines

Self explanatory. The elide points toward the caret with `...`.

```text
  at wide.rp:2:196
      ... + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
                                                   ^ expected *i8, found i32
```

### 7. Helpers

There are a bunch of helper functions under `diagnostic.ml` that range from common user errors to internal compiler errors. I only write a helper function if there are two or more call sites. For the one off errors, I use the pipeline.

## Testing

I create expect tests for every diagnostic to create a snapshot of
the messaging and the span offset.

```sh
dune test --auto-promote
python3 test/programs/run.py --promote
```
