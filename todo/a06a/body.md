A broken var init skips the `;` check even when the next line is fine.

var x = @
y = 6;

error: unexpected character
  at a.rp:2:11
error: undefined variable
  at a.rp:3:3

var x = "
y = 6;

error: unterminated string
  at b.rp:2:11
error: undefined variable
  at b.rp:3:3

var x = ""
y = 6;

error: expected `;`
  at c.rp:3:3
error: undefined variable
  at c.rp:3:3

var x = @ y = 6;

error: unexpected character
  at d.rp:2:11
error: undefined variable
  at d.rp:2:13

fn main() i32
  var x = "
  p = 5
}

error: expected `{`
  at main.rp:2:3
error: unterminated string
  at main.rp:2:11

This one shows both now. Rust, Zig and Go sometimes drop one.
