Stack alloc grows the frame inside loops, `var` and `const` and the for loop slots still alloc in the body, hoist to `@start` respecting shadowing and block scoping
