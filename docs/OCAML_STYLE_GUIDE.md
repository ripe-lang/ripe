# Ripe's OCaml Style Guide

- You should run `dune fmt` and leave layout to it

- You should write the simple version first and measure before making it faster

- You should keep single use code inline and only pull out a helper once a second caller needs it

```ocaml
let rec resolve_ty = function TAlias (_, base) -> resolve_ty base | t -> t
```

- You should pass a function everything it reads or writes except the Interner and Diagnostic, which is the one system every phase reports user errors, warnings and internal errors through

```ocaml
Diagnostic.emit (Diagnostic.error f.field_name.span "duplicate field")
```

- You should take the one value a function needs instead of the record that holds it

```ocaml
read_string (Buffer.create 16) lexbuf
```

- You should put what a function needs in its types so a caller can't hand it something wrong

```ocaml
and path = { owner : name spanned Nonempty.t; member : name spanned }
```

- You should keep the code that builds a value apart from the code that prints or emits it

```ocaml
Output.text output (file_tokens filename)
```

- You should only write a type annotation when the compiler can't tell which record or constructor you mean

```ocaml
let desc (operand : operand) = operand.desc
```

- You should open the modules a file is built around at the top and give each constructor a name no other opened module uses

```ocaml
open Types
open Tast

let lit n = mk (TInt I32) (TIntLit n)
```

- You should use `function` when the body only matches the last argument and no arm needs it

```ocaml
let show_unop = function Neg -> "-" | Not -> "!" | BitNot -> "~"
```

- You should wrap a multi line `then` or `else` in `begin` and `end` instead of parentheses

```ocaml
if at st COMMA then begin
  advance st;
```

- You should only use `==` to ask if two values are the same object or to compare tokens with no payload

```ocaml
let at st t = cur_token st == t
```

- You should put the main types at the top of a file and a small type right above its only user

```ocaml
type visit = On_path | Finished
let cyclic_structs defs iter_edges = ...
```

- You should make a record field `mutable` instead of holding a `ref` unless copies made with `with` need to share it

```ocaml
mutable current : block_id;
```

- You should walk to the end of a list to read its last element instead of reversing it

```ocaml
| [ te ] -> te.ty
| _ :: rest -> tblock_ty rest
```

- You should put a named helper at the top level instead of nesting it inside another function and only keep a local `let rec go` for the function's own loop

```ocaml
let rec after_stars st index =
```

- You should start each `parse_*` with the Ripe it reads and each `lower_*` with the MIR it emits

```ocaml
(* Point :: struct { x: i32, y: i32 } *)
(* %2 = copy %0 + copy %1 *)
```

