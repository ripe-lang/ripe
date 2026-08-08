# Diagnostics

A design document of how to use `diagnostic.ml`.

## Template

```
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

Below is the **primary label** that carries the values.

```text
cannot apply `+` to bool
expected `;`
never is only valid as a function return type 
```

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
      let _y = *n.x + 1
                ^~~ on i32
```

### 3. Spans

The point is to be obvious and minimal about where the fault is at.

```text
error: no field
    return p.zebra
             ^~~~~ on struct pt
```

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

### 6. Helpers

There are a bunch of helper functions under `diagnostic.ml` that range from common user errors to internal compiler errors. I only write a helper function if there are two or more call sites. For the one off errors, I use the pipeline.

## Testing

I create expect tests for every diagnostic to create a snapshot of
the messaging and the span offset.

```sh
dune test --auto-promote
python3 test/programs/run.py --promote
```
