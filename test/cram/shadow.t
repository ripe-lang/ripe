Shadowing and local/global resolution must survive codegen. 

  $ run() {
  >   printf '%s' "$2" > p.rp
  >   ripec -o ./p p.rp 2>/dev/null && ./p
  >   echo "[$1 exit=$?]"
  > }

An inner block shadows the outer binding and the outer is read afterward.

  $ run nested_block 'func main() i32 {
  >   var x: i32 = 1
  >   { var x: i32 = 2 }
  >   return x
  > }'
  [nested_block exit=1]

A same scope re-declaration shadows and its initializer reads the old binding.

  $ run same_scope 'func main() i32 {
  >   var x: i32 = 1
  >   var x: i32 = x + 4
  >   return x
  > }'
  [same_scope exit=5]

The loop variable is scoped to the loop and leaves the outer name untouched.

  $ run for_var 'func main() i32 {
  >   var i: i32 = 99
  >   for i in 0..3 { }
  >   return i
  > }'
  [for_var exit=99]

A shadow inside an if branch does not leak, so a later write lands on the global.

  $ run global_not_clobbered 'var g: i32 = 0
  > func bump(c: bool) {
  >   if c { var g: i32 = 5 }
  >   g = 10
  > }
  > func main() i32 {
  >   bump(true)
  >   return g
  > }'
  [global_not_clobbered exit=10]

A function scope local shadows a global of the same name.

  $ run local_over_global 'var g: i32 = 3
  > func main() i32 {
  >   var g: i32 = 7
  >   g = g + 1
  >   return g
  > }'
  [local_over_global exit=8]
