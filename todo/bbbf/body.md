```ripe
var s: str;
io.print_int(cast(i64, s.len));   // 3735936685 (0xDEADBEAD)
```

QBE stores only the ptr word (`storel 0, %s`) and never the len at offset 8. Slices and structs store both.
