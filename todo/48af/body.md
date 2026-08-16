Keep zero initialization. undefined still errors. Reads error at the read. var_info needs a mutable type and pending flag. check_block patches pending bindings when the block closes.

func f() {
  var x
  x = 42
}
