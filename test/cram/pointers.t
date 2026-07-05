Assignment through pointers and struct fields must survive codegen.

  $ run() {
  >   printf '%s' "$2" > p.rp
  >   ripec -o ./p p.rp 2>/dev/null && ./p
  >   echo "[$1 exit=$?]"
  > }

A bare pointer deref assignment actually stores, instead of compiling to nothing.

  $ run deref_assign 'func main() i32 {
  >   var x: i32 = 10
  >   var q: *i32 = &x
  >   *q = 42
  >   return x
  > }'
  [deref_assign exit=42]

A direct struct field assignment.

  $ run field_assign 'struct point { x: i32, y: i32 }
  > func main() i32 {
  >   var p: point = point { x: 1, y: 2 }
  >   p.x = 7
  >   return p.x
  > }'
  [field_assign exit=7]

A struct field assignment through a pointer, the original repro for this bug.

  $ run field_assign_via_ptr 'struct point { x: i32, y: i32 }
  > func main() i32 {
  >   var p: point = point { x: 34, y: 56 }
  >   var pt: *point = &p
  >   pt.x = 8
  >   return p.x
  > }'
  [field_assign_via_ptr exit=8]

Compound assignment through a pointer to a field.

  $ run compound_field_via_ptr 'struct point { x: i32, y: i32 }
  > func main() i32 {
  >   var p: point = point { x: 10, y: 0 }
  >   var pt: *point = &p
  >   pt.x += 5
  >   return p.x
  > }'
  [compound_field_via_ptr exit=15]

A nested field write through a pointer, two levels deep.

  $ run nested_field_via_ptr 'struct inner { a: i32 }
  > struct outer { i: inner, b: i32 }
  > func main() i32 {
  >   var o: outer = outer { i: inner { a: 1 }, b: 2 }
  >   var po: *outer = &o
  >   po.i.a = 99
  >   return o.i.a
  > }'
  [nested_field_via_ptr exit=99]

Assigning a whole struct through a deref, not just a scalar field.

  $ run whole_struct_via_deref 'struct point { x: i32, y: i32 }
  > func main() i32 {
  >   var p: point = point { x: 1, y: 2 }
  >   var pt: *point = &p
  >   var p2: point = point { x: 100, y: 200 }
  >   *pt = p2
  >   return p.x
  > }'
  [whole_struct_via_deref exit=100]

Taking the address of a field or a parenthesized deref, not just a plain name.

  $ run address_of_field 'struct point { x: i32, y: i32 }
  > func main() i32 {
  >   var p: point = point { x: 7, y: 0 }
  >   var px: *i32 = &(p.x)
  >   return *px
  > }'
  [address_of_field exit=7]

  $ run address_of_deref 'struct point { x: i32, y: i32 }
  > func main() i32 {
  >   var p: point = point { x: 3, y: 0 }
  >   var pt: *point = &p
  >   var pt2: *point = &(*pt)
  >   return pt2.x
  > }'
  [address_of_deref exit=3]
