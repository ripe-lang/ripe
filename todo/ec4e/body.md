var a: i32 = 1 / 0
var b: i32 = 1 / 0

Only a's error prints and b's error is skipped. The problem I think is global const eval happens during MIR building so an earlier type error stops the run before later const errors are seen. So I should probably move it into type checking so one run reports all of them.
