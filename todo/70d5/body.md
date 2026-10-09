The lexer should say `integer literal exceeds u64 range` or something for `18446744073709551616`. It should be different from the default i32 error for `2147483648`.
