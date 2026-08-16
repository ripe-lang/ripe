var small: u32 = 5
var same: u32 = small as u32   // same type on both sides, does nothing

This has nothing to do with implicit widening. When you cast a value to the type it already has was never required by anything and never will be. It just compiles silently with zero warning today.
