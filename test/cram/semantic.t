Compile and run programs with a known correct exit code, so a semantic
regression fails here instead of only showing up as a wrong number downstream.

  $ run() {
  >   printf '%s' "$2" > p.rp
  >   ripec -o ./p p.rp 2>/dev/null && ./p
  >   echo "[$1 exit=$?]"
  > }

Signed division truncates toward zero.

  $ run signed_div_trunc 'func main() i32 {
  >   var a: i32 = -7
  >   return a / 2
  > }'
  [signed_div_trunc exit=253]

Signed modulo takes the sign of the dividend.

  $ run signed_mod 'func main() i32 {
  >   var a: i32 = -7
  >   return a % 2
  > }'
  [signed_mod exit=255]

An arithmetic right shift on a negative number sign extends.

  $ run sar_shift 'func main() i32 {
  >   var a: i32 = -8
  >   return a >> 1
  > }'
  [sar_shift exit=252]

An unsigned compare treats the top bit as magnitude, not sign.

  $ run unsigned_cmp 'func main() i32 {
  >   var a: u32 = 4294967295
  >   if a > 1 { return 1 }
  >   return 0
  > }'
  [unsigned_cmp exit=1]

A slice write reaches back into the backing array.

  $ run slice_write_through 'func main() i32 {
  >   var a: [4]i32 = [1, 2, 3, 4]
  >   var sl: []i32 = a[0..4]
  >   sl[2] = 9
  >   return a[2]
  > }'
  [slice_write_through exit=9]

Iterating a slice visits the sliced range, not the whole array.

  $ run slice_iter 'func main() i32 {
  >   var a: [4]i32 = [1, 2, 3, 4]
  >   var sl: []i32 = a[1..3]
  >   var s: i32 = 0
  >   for x in sl { s += x }
  >   return s
  > }'
  [slice_iter exit=5]

A 2D array indexes row major and a write only touches its own cell.

  $ run 2d_array_write 'func main() i32 {
  >   var m: [2][2]i32 = [[1, 2], [3, 4]]
  >   m[1][0] = 7
  >   return m[1][0] + m[0][1]
  > }'
  [2d_array_write exit=9]

Assigning an array literal into a row copies its cells, not a pointer.

  $ run 2d_row_assign 'func main() i32 {
  >   var m: [2][2]i32 = [[1, 2], [3, 4]]
  >   m[0] = [9, 8]
  >   return m[0][0] + m[0][1]
  > }'
  [2d_row_assign exit=17]

Assigning one row into another copies its bytes through.

  $ run 2d_row_copy 'func main() i32 {
  >   var m: [2][2]i32 = [[1, 2], [3, 4]]
  >   m[0] = m[1]
  >   return m[0][0] + m[0][1]
  > }'
  [2d_row_copy exit=7]

A function pointer variable can be reassigned and called through.

  $ run funcptr_var 'func f() i32 { return 3 }
  > func g() i32 { return 4 }
  > func main() i32 {
  >   var h: () i32 = f
  >   var r1: i32 = h()
  >   h = g
  >   return r1 + h()
  > }'
  [funcptr_var exit=7]

A function pointer passed as a parameter is called through the parameter.

  $ run funcptr_param 'func f(x: i32) i32 { return x + 1 }
  > func apply(fn: (i32) i32, v: i32) i32 { return fn(v) }
  > func main() i32 { return apply(f, 4) }'
  [funcptr_param exit=5]

Call arguments evaluate left to right.

  $ run arg_eval_order 'extern func printf(fmt: cstr, ...) i32
  > var log: i32 = 0
  > func mark(v: i32) i32 { log = log * 10 + v; return v }
  > func take3(a: i32, b: i32, c: i32) i32 { return 0 }
  > func main() i32 {
  >   take3(mark(1), mark(2), mark(3))
  >   return log
  > }'
  [arg_eval_order exit=123]

break exits the innermost loop and continue skips to the next iteration.

  $ run break_continue 'func main() i32 {
  >   var s: i32 = 0
  >   for i in 0..10 {
  >     if i == 3 { continue }
  >     if i == 6 { break }
  >     s += i
  >   }
  >   return s
  > }'
  [break_continue exit=12]

A break only exits its own loop and the outer loop keeps going.

  $ run nested_loop_break 'func main() i32 {
  >   var s: i32 = 0
  >   for i in 0..3 {
  >     for j in 0..10 {
  >       if j == 2 { break }
  >       s += 1
  >     }
  >   }
  >   return s
  > }'
  [nested_loop_break exit=6]

A float to int cast truncates toward zero.

  $ run float_to_int_cast 'func main() i32 {
  >   var f: f64 = 2.5
  >   var g: f64 = f * 2.0
  >   return g as i32
  > }'
  [float_to_int_cast exit=5]

fib(10) by naive recursion.

  $ run recursion_fib 'func fib(n: i32) i32 {
  >   if n < 2 { return n }
  >   return fib(n - 1) + fib(n - 2)
  > }
  > func main() i32 { return fib(10) }'
  [recursion_fib exit=55]

An array parameter is passed by value and a callee write does not escape.

  $ run array_param_byval 'func clobber(a: [3]i32) { a[0] = 99 }
  > func main() i32 {
  >   var a: [3]i32 = [1, 2, 3]
  >   clobber(a)
  >   return a[0]
  > }'
  [array_param_byval exit=1]

A global mutated by one call is read back by the next.

  $ run global_mutate 'var counter: i32 = 5
  > func bump() { counter += 3 }
  > func main() i32 {
  >   bump()
  >   bump()
  >   return counter
  > }'
  [global_mutate exit=11]

elseif chains pick the first matching branch and later branches never run.

  $ run elseif_chain 'func classify(x: i32) i32 {
  >   if x < 0 { return 0 }
  >   elseif x == 0 { return 1 }
  >   elseif x < 10 { return 2 }
  >   else { return 3 }
  > }
  > func main() i32 { return classify(5) }'
  [elseif_chain exit=2]

An unsigned right shift fills with zero instead of the sign bit.

  $ run logical_shift_u32 'func main() i32 {
  >   var a: u32 = 4294967288
  >   var b: u32 = a >> 1
  >   if b == 2147483644 { return 1 }
  >   return 0
  > }'
  [logical_shift_u32 exit=1]

A left shift on an i64 clears the low bits across the full 64 bit width.

  $ run i64_shift_wide 'func main() i32 {
  >   var a: i64 = 1
  >   var b: i64 = a << 40
  >   if b == 1099511627776 { return 1 }
  >   return 0
  > }'
  [i64_shift_wide exit=1]

sizeof a struct accounts for field alignment padding.

  $ run sizeof_struct_pad 'struct p { a: i8, b: i64, c: i8 }
  > func main() i32 { return sizeof(p) as i32 }'
  [sizeof_struct_pad exit=24]

sizeof an array is element size times length.

  $ run sizeof_array 'func main() i32 { return sizeof([5]i16) as i32 }'
  [sizeof_array exit=10]

A type alias behaves exactly like the type it names.

  $ run type_alias 'type myint = i64
  > func main() i32 {
  >   var x: myint = 9
  >   return x as i32
  > }'
  [type_alias exit=9]

A newtype cast to and from its base type is a no-op bit copy at runtime.

  $ run newtype_cast 'newtype Celsius = f32
  > func to_f(c: Celsius) f32 { return (c as f32 * 1.8 + 32.0) }
  > func main() i32 { return to_f(100.0 as Celsius) as i32 }'
  [newtype_cast exit=212]

An assignment expression yields the assigned value, so a chained assign
sets both names.

  $ run assign_expr_value 'func main() i32 {
  >   var x: i32 = 0
  >   var y: i32 = 0
  >   x = y = 5
  >   return x + y
  > }'
  [assign_expr_value exit=10]

isize and usize behave like ordinary integer types in arithmetic.

  $ run isize_usize 'func main() i32 {
  >   var a: isize = 40
  >   var b: usize = 2
  >   return (a as i32) + (b as i32)
  > }'
  [isize_usize exit=42]

A global array literal reads back the element at its index.

  $ run global_array 'var table: [3]i32 = [10, 20, 30]
  > func main() i32 { return table[1] }'
  [global_array exit=20]

fib(10) run through a function pointer stored in a param, not a direct call.

  $ run funcptr_recursion_helper 'func fib(n: i32) i32 {
  >   if n < 2 { return n }
  >   return fib(n - 1) + fib(n - 2)
  > }
  > func apply(fn: (i32) i32, v: i32) i32 { return fn(v) }
  > func main() i32 { return apply(fib, 9) }'
  [funcptr_recursion_helper exit=34]

A while loop counts up to its bound.

  $ run while_loop 'func main() i32 {
  >   var i: i32 = 0
  >   while i < 7 { i += 1 }
  >   return i
  > }'
  [while_loop exit=7]

A cstr round trips through an extern strlen call.

  $ run cstr_strlen 'extern func strlen(s: cstr) i64
  > func main() i32 {
  >   var s: cstr = "hello"
  >   return strlen(s) as i32
  > }'
  [cstr_strlen exit=5]

A float passed to a variadic printf reads back correctly, which needs the
... marker so the vararg register count is set.

  $ run printf_float_vararg 'extern func printf(fmt: cstr, ...) i32
  > func main() i32 {
  >   printf("%.2f\n", 3.14)
  >   return 0
  > }'
  3.14
  [printf_float_vararg exit=0]

A bare return in main exits with 0 like falling off the end.

  $ run bare_return_main 'func main() i32 {
  >   if true { return }
  >   return 1
  > }'
  [bare_return_main exit=0]

A u64 literal in the top half is emitted as an all ones constant, so adding
one wraps back to zero.

  $ run u64_max_wraps 'func main() i32 {
  >   var x: u64 = 18446744073709551615
  >   var y: u64 = x + 1
  >   if y == 0 { return 7 }
  >   return 0
  > }'
  [u64_max_wraps exit=7]
