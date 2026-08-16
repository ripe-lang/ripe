var a: i8 = 100 + 100   // typechecks today and silently wraps to -56
var b: u8 = 200 + 100   // typechecks today and silently wraps to 44

Right now the range only check fires at the leaf so every 100 passes on its own and the folded 200 is never checked against the target
