(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers
module C = Ripe.Core

let rec dump_stmt (st : C.cstmt) : string =
  match st.C.tsdesc with
  | C.CBinding (_, s, _, _) -> "bind " ^ s.Ripe.Symbol.name
  | C.CExpr _ -> "expr"
  | C.CReturn _ -> "return"
  | C.CBreak -> "break"
  | C.CContinue -> "continue"
  | C.CIf (branches, else_body) ->
      let arm (_, body) = "if " ^ dump_stmts body in
      String.concat " " (List.map arm branches)
      ^ " else " ^ dump_stmts else_body
  | C.CLoop body -> "loop " ^ dump_stmts body

and dump_stmts (stmts : C.cstmt list) : string =
  "{ " ^ String.concat " " (List.map dump_stmt stmts) ^ " }"

let run_lower src =
  let decls = parse src in
  let uses = Ripe.Resolve.resolve decls in
  let tdecls, _ = Ripe.Typechecker.typecheck uses decls in
  let cdecls = Ripe.Lower.lower tdecls in
  List.iter
    (function
      | C.CFunc fd -> print_endline (fd.C.name ^ " " ^ dump_stmts fd.C.body)
      | _ -> ())
    cdecls

let%expect_test "lower: block statements land in the surrounding list" =
  run_lower
    {|
func f() i32 {
  var x: i32 = 1
  {
    var y: i32 = 2
    x = y
  }
  return x
}
|};
  [%expect {| f { bind x bind y expr return } |}]

let%expect_test "lower: compound assignment splices flat" =
  run_lower "func f() i32 { var x: i32 = 1 x += 2 return x }";
  [%expect {| f { bind x bind compound.p.0 expr return } |}]
