needs a loss policy (trap on fraction/precision loss or only overflow) then a round trip compare with NaN and inf guards. e.g. `2.5 as! i32` and `big_i64 as! f32`
