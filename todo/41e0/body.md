  | T.TSizeOf t -> string_of_int (ty_size ctx.structs t)
(* TODO: explicit deref on a struct pointer (p^.x) emits an extra loadl, fix once struct value semantics are implemented.