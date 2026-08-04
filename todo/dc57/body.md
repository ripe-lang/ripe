Writing to a by value array or struct parameter only touches the private callee copy so the compiler can see the dead store and should report it. Slices don't copy so they're exempt.
