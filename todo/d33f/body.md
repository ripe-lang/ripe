`as bool` is a plain copy not normalized to 0 or 1 so `256 as bool` is truthy in a register but stores byte 0, lower it to `cne 0` or reject casts to bool
