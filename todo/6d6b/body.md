var inferred = 100000 * 100000   // compiles and gives 1410065408

No annotation, no target type so both literals fall back to i32 and the product wraps silently. rustc rejects the same line at compile time.
