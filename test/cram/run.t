Compile and run every example. A pipeline regression flips the snapshot.

  $ for f in ../../examples/*.rp; do
  >   name=$(basename "$f" .rp)
  >   ripec -o "./$name" "$f" 2>/dev/null && "./$name"
  >   echo "[$name exit=$?]"
  > done
  [array_search exit=6]
  [array_stats exit=4]
  [arrays exit=34]
  [bitops exit=1]
  [control_flow exit=55]
  [fn_ptr exit=42]
  hello ramon, you are 22 years old
  [io exit=0]
  [main exit=42]
  [matrix exit=15]
  [pointers exit=42]
  [structs exit=21]
  [undefined exit=4]
