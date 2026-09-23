# Test

The unit tests live in the `test_*.ml` files and look like this:

```ocaml
let%expect_test "lexer: semicolon inserted after expression newline" =
  dump_tokens "x\n";
  [%expect {|
    IDENT x
    AUTOSEMI
    EOF
    |}]
```

```
$ dune test
```
