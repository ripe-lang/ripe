Variadic f32 args skip the C float to double promotion so `printf("%f\n", x)` with `x: f32` prints `0.000000`, cast float varargs to f64 in `check_args`
