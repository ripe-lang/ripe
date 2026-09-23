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

You can use `fuzz` to break working programs and look for bad error messages
and `programs` to run whole programs and check what they print against a saved
copy.

```
$ dune test
```
