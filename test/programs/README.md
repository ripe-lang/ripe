# Programs

Each test is a directory with a `main.rp` and the output it should give you:

```
semantic/sizeof_array/
  main.rp          func main() i32 { return sizeof([5]i16) }
  out.txt          exit: 10

recovery/alias_type_eats_next/
  main.rp          type a = b: i32
  compilererr.txt  error: expected type
                     at main.rp:1:8
                       type a = b: i32
                              ^
```

Some tests also pass extra flags to `ripec` with a `flags.txt`, link in `*.c` files, or import other `*.rp` modules. A `// BROKEN: <reason>` line at the top of `main.rp` means the test is known to fail.

```
$ python3 test/programs/run.py recovery
```
