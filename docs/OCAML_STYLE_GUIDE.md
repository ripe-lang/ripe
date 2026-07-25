# Ripe's OCaml Style Guide

- Keep code simple and readable because people read code more often than they write it

```ocaml
let is_valid_literal literal =
  literal <> ""
```

- Use spaces instead of tabs and indent with 2 spaces

```ocaml
let total values =
  List.fold_left ( + ) 0 values
```

- Keep lines to 80 characters or less and break long expressions

```ocaml
let result =
  transform input
  |> validate
  |> lower
```

- Use snake_case for values functions types and record fields

```ocaml
type source_file = { file_name : string; contents : string }
let file_name source = source.file_name
```

- Use PascalCase for constructors modules and signatures and uppercase names for module types

```ocaml
type result = Success of string | Failure of string
module Parser = struct end
module type PARSER = sig end
```

- Use short names in small scopes and descriptive names in public interfaces

```ocaml
let span_width span =
  span.hi - span.lo
```

- Add type annotations to top level functions and values

```ocaml
let is_valid_identifier (name : string) : bool =
  name <> ""
```

- You should use `function` for direct matches and an explicit argument with `match` when the name or type could use some clarification

```ocaml
let describe value =
  match value with
  | Some text -> text
  | None -> "none"

let describe = function
  | Some text -> text
  | None -> "none"
```

- Name the main type `t` inside its module

```ocaml
module Symbol = struct
  type t = { name : string }
end
```

- Order module declarations: 1. exceptions 2. types 3. modules 4. values.

```ocaml
module Store = struct
  exception Missing
  type t = string list
  module Internal = struct end
  let empty = []
end
```

- Use comments to explain WHY the code exists

```ocaml
(* Save the first diagnostic so later phases can report all related errors *)
let first_diagnostic diagnostics =
  match diagnostics with
  | diagnostic :: _ -> Some diagnostic
  | [] -> None
```

- Don't add comments that repeat the code

```ocaml
let count = List.length values
```

- Use immutable data and pure functions and keep mutable data local

```ocaml
let increment value = value + 1
```

- Keep mutable values and references out of global scope unless you have a good reason to use them

```ocaml
let make_counter () =
  let count = ref 0 in
  fun () ->
  let () = incr count in
    !count
```

- Use regular variants by default and polymorphic variants when their flexibility helps

```ocaml
type token_kind = Identifier | Keyword | Punctuation
```

- Use pattern matching instead of nested field access or nested conditionals

```ocaml
let first = function
  | item :: _ -> item
  | [] -> ""
```

- Make pattern matches exhaustive and name the constructors you expect instead of using `_` (I still have a long way to go on this one)

```ocaml
let label = function
  | Identifier -> "identifier"
  | Keyword -> "keyword"
  | Punctuation -> "punctuation"
```

- Match related values together to avoid nesting

```ocaml
let classify = function
  | None -> "empty"
  | Some 0 -> "zero"
  | Some _ -> "other"
```

- Use `if` for booleans and `match` for values with multiple constructors

```ocaml
let phase_status complete = if complete then "done" else "pending"
```

- Use `List.hd` and `List.tl` only when you want an exception or match the list directly

```ocaml
let first = function
  | [] -> None
  | item :: _ -> Some item
```

- Use standard iterators and folds instead of writing the same traversal yourself

```ocaml
let names = List.map String.length values
```

- Give complex arguments and anonymous functions a name before passing them to an iterator

```ocaml
let normalize_token value = String.lowercase_ascii value
let tokens = List.map normalize_token values
```

- Put the data structure argument last

```ocaml
let lengths = List.map String.length names
```

- Use labeled arguments when same typed arguments could be mixed up

```ocaml
let between ~low ~high value = low <= value && value <= high
```

- Don't use global opens and use qualified names or local opens

```ocaml
let size declarations = List.length declarations
let value =
  let open Int64 in
  add 1L 2L
```

- Don't use double semicolons

```ocaml
let answer = 42
```

- Use `=` and `<>` for structural equality and `==` and `!=` for physical equality

```ocaml
if left = right then equal else different
```

- Use `begin` and `end` around multiple expressions in an `if` branch with side effects

```ocaml
if enabled then begin
  let () = write_first () in
  write_second ()
end
```

- Use tail recursion for large inputs and check allocation costs

```ocaml
let length values =
  let rec loop count = function
    | [] -> count
    | _ :: rest -> loop (count + 1) rest
  in
  loop 0 values
```

- Keep functions small and put repeated logic in a helper

```ocaml
let is_valid_declaration value = value <> ""
let valid_declarations values = List.filter is_valid_declaration values
```

- Give magic numbers and other unclear literals a name

```ocaml
let bits_per_word = 64
```

- Use parentheses only when they affect parsing or make grouping clearer

```ocaml
match value with
| Some item ->
  begin match item with
  | 0 -> "zero"
  | _ -> "other"
  end
| None -> "missing"
```

## Sources

1. [OCaml Towards Clarity and Grace](https://github.com/lindig/ocaml-style/blob/master/README.md)
2. [CS3110 OCaml Style Guide](https://www.cs.cornell.edu/courses/cs3110/2011sp/Handouts/style.htm)
3. [OCaml Programming Guidelines](https://ocaml.org/docs/guidelines)
