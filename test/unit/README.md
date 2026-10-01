# Test

The unit tests live in the `test_*.ml` files and look like this:

```ocaml
let%expect_test "lexer: string is one token" =
  dump_tokens {|"hello"|};
  [%expect {|
    STRING hello
    EOF
    |}]
```

```
$ dune test
```
