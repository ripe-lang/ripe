A broken binding init skips the `;` check even when the next line is fine. The parser stops at the first error now so only the first one shows until recovery comes back.

x := @
y = 6;

error: unexpected character
  at a.rp:2:8

x := "
y = 6;

error: unterminated string
  at b.rp:2:8

x := ""
y = 6;

error: expected `;`
  at c.rp:3:3

x := @ y = 6;

error: unexpected character
  at d.rp:2:8

fn main() i32
  x := "
  p = 5
}

error: expected `{`
  at main.rp:2:3

This one used to show the unterminated string too. Rust, Zig and Go sometimes drop one.
