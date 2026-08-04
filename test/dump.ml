(* SPDX-License-Identifier: GPL-2.0-only *)

module C = Ripe.Core

let tok_str (t : Ripe.Tokens.token) =
  let open Ripe.Tokens in
  match t with
  | IDENT s -> "IDENT " ^ s
  | INT (n, suf) -> "INT " ^ Int64.to_string n ^ Option.value ~default:"" suf
  | FLOAT f -> "FLOAT " ^ string_of_float f
  | STRING s -> "STRING " ^ String.escaped s
  | SEMI -> "SEMI"
  | EOF -> "EOF"
  | ERROR s -> "ERROR " ^ s
  | other ->
      if List.exists (fun (_, t') -> t' = other) keywords then
        "KW " ^ show_token other
      else show_token other

(* a compact s-expr for expr trees with no spans *)
let rec dump_typ (t : Ripe.Ast.typ) =
  match t.tdesc with
  | ErrorType -> "<error>"
  | Named (path, n) -> Ripe.Ast.show_named path n
  | Pointer p -> "*" ^ dump_typ p
  | Array (n, t) -> "[" ^ dump_expr n ^ "]" ^ dump_typ t
  | Slice t -> "[]" ^ dump_typ t
  | FuncPtr (params, ret) ->
      let ps = String.concat ", " (List.map dump_typ params) in
      let r = match ret with Some t -> " " ^ dump_typ t | None -> "" in
      "(" ^ ps ^ ")" ^ r

and dump_expr (e : Ripe.Ast.expr) =
  match e.desc with
  | ErrorExpr -> "<error>"
  | Int (n, suf) -> Int64.to_string n ^ Option.value ~default:"" suf
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Null -> "null"
  | Char c -> Printf.sprintf "'\\u{%X}'" c
  | String s -> "\"" ^ s ^ "\""
  | Ident s -> s
  | Call (callee, args) ->
      "(call " ^ dump_expr callee
      ^ String.concat "" (List.map (fun a -> " " ^ dump_expr a) args)
      ^ ")"
  | BinOp (op, l, r) ->
      "(" ^ Ripe.Ast.show_binop_sym op ^ " " ^ dump_expr l ^ " " ^ dump_expr r
      ^ ")"
  | UnOp (op, e) -> "(" ^ Ripe.Ast.show_unop_sym op ^ " " ^ dump_expr e ^ ")"
  | Range (l, r) -> "(.. " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | RangeInclusive (l, r) -> "(..= " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | FieldAccess (e, f) -> "(. " ^ dump_expr e ^ " " ^ f ^ ")"
  | Cast (e, t, kind) ->
      "(" ^ Ripe.Ast.show_cast_op kind ^ " " ^ dump_expr e ^ " " ^ dump_typ t
      ^ ")"
  | SizeOf t -> "(sizeof " ^ dump_typ t ^ ")"
  | ArrayLit elems ->
      "(array"
      ^ String.concat "" (List.map (fun e -> " " ^ dump_expr e) elems)
      ^ ")"
  | Index (base, idx) -> "(index " ^ dump_expr base ^ " " ^ dump_expr idx ^ ")"
  | Undefined -> "undefined"
  | StructLit (path, name, _, fields) ->
      "(struct "
      ^ Ripe.Ast.show_named path name
      ^ String.concat ""
          (List.map
             (fun (f, _, e) -> " (" ^ f ^ " " ^ dump_expr e ^ ")")
             fields)
      ^ ")"
  | Block body -> dump_block body
  | If (branches, else_body) ->
      let arm (c, body) = "(" ^ dump_expr c ^ " " ^ dump_block body ^ ")" in
      "(if "
      ^ String.concat " " (List.map arm branches)
      ^ (match else_body with Some b -> " " ^ dump_block b | None -> "")
      ^ ")"
  | While (c, body) -> "(while " ^ dump_expr c ^ " " ^ dump_block body ^ ")"
  | For (name, _, iter, body) ->
      "(for " ^ name ^ " " ^ dump_expr iter ^ " " ^ dump_block body ^ ")"
  | Binding (_, name, _, _, init) ->
      "(let " ^ name
      ^ (match init with Some e -> " " ^ dump_expr e | None -> "")
      ^ ")"
  | Return e ->
      "(return"
      ^ (match e with Some e -> " " ^ dump_expr e | None -> "")
      ^ ")"
  | Break -> "(break)"
  | Continue -> "(continue)"
  | PairAssign (ft, st, fv, sv) ->
      "(pair " ^ String.concat " " (List.map dump_expr [ ft; st; fv; sv ]) ^ ")"

and dump_block (body : Ripe.Ast.block) : string =
  "(block " ^ String.concat " " (List.map dump_expr body) ^ ")"

(* a compact core dump where the flat list is the whole point *)
let rec dump_cstmt (e : C.cexpr) : string =
  match e.C.desc with
  | C.CBinding (_, s, _, _) -> "bind " ^ s.Ripe.Symbol.name
  | C.CReturn _ -> "return"
  | C.CBreak -> "break"
  | C.CContinue -> "continue"
  | C.CIf (branches, else_body) -> (
      let arm (_, body) = "if " ^ dump_cstmts body in
      String.concat " " (List.map arm branches)
      ^ match else_body with Some b -> " else " ^ dump_cstmts b | None -> "")
  | C.CLoop body -> "loop " ^ dump_cstmts body
  | C.CBlock body -> "block " ^ dump_cstmts body
  | _ -> "expr"

and dump_cstmts (stmts : C.cblock) : string =
  "{ " ^ String.concat " " (List.map dump_cstmt stmts) ^ " }"

let dump_tokens src =
  let st = Ripe.Lexer.make_state 0 in
  let lexbuf = Lexing.from_string src in
  let rec go () =
    let t, _ = Ripe.Lexer.read st lexbuf in
    print_endline (tok_str t);
    if t <> Ripe.Tokens.EOF then go ()
  in
  go ()
