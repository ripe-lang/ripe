only a bare literal pierces a newtype at a binding, an arithmetic expression like 20 + 4 still errors
newtype Meters = i32
const m: Meters = 24      // works, bare literal adopts
const m: Meters = 20 + 4  // errors, expected Meters found i32
