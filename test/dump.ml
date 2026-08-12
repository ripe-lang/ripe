(* SPDX-License-Identifier: GPL-2.0-only *)

let tok_str (t : Ripe.Tokens.token) =
  let open Ripe.Tokens in
  match t with
  | IDENT s -> "IDENT " ^ s
  | INT (n, suf) -> "INT " ^ Int64.to_string n ^ Option.value ~default:"" suf
  | FLOAT (f, suf) ->
      "FLOAT " ^ string_of_float f ^ Option.value ~default:"" suf
  | STRING s -> "STRING " ^ String.escaped s
  | AUTOSEMI -> "AUTOSEMI"
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
  | FuncPtr (_, params, ret) ->
      let ps = String.concat ", " (List.map dump_typ params) in
      let r = match ret with Some t -> " " ^ dump_typ t | None -> "" in
      "(" ^ ps ^ ")" ^ r

and dump_expr (e : Ripe.Ast.expr) =
  match e.desc with
  | ErrorExpr -> "<error>"
  | Int (n, suf) -> Int64.to_string n ^ Option.value ~default:"" suf
  | Float (f, suf) -> string_of_float f ^ Option.value ~default:"" suf
  | Bool b -> string_of_bool b
  | Null -> "null"
  | Char c -> Printf.sprintf "'\\u{%X}'" c
  | String s -> "\"" ^ s ^ "\""
  | Ident s -> Ripe.Interner.text s
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
  | RangeFrom l -> "(.. " ^ dump_expr l ^ ")"
  | RangeTo r -> "(.. " ^ dump_expr r ^ ")"
  | RangeToInclusive r -> "(..= " ^ dump_expr r ^ ")"
  | RangeFull -> "(..)"
  | FieldAccess (e, f, _) ->
      "(. " ^ dump_expr e ^ " " ^ Ripe.Interner.text f ^ ")"
  | Cast (e, t) -> "(as " ^ dump_expr e ^ " " ^ dump_typ t ^ ")"
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
             (fun (f, _, e) ->
               " (" ^ Ripe.Interner.text f ^ " " ^ dump_expr e ^ ")")
             fields)
      ^ ")"
  | Block body -> dump_block body
  | If (branches, else_body) ->
      let arm (c, body) =
        "(" ^ dump_expr c ^ " " ^ dump_block body.Ripe.Ast.value ^ ")"
      in
      "(if "
      ^ String.concat " " (List.map arm branches)
      ^ (match else_body with
        | Some b -> " " ^ dump_block b.Ripe.Ast.value
        | None -> "")
      ^ ")"
  | While (_, c, body) -> "(while " ^ dump_expr c ^ " " ^ dump_block body ^ ")"
  | Loop (_, body) -> "(loop " ^ dump_block body ^ ")"
  | For (_, name, _, iter, body) ->
      "(for " ^ Ripe.Interner.text name ^ " " ^ dump_expr iter ^ " "
      ^ dump_block body ^ ")"
  | Binding (_, name, _, _, init) ->
      "(var " ^ Ripe.Interner.text name
      ^ (match init with Some e -> " " ^ dump_expr e | None -> "")
      ^ ")"
  | Return e ->
      "(return"
      ^ (match e with Some e -> " " ^ dump_expr e | None -> "")
      ^ ")"
  | Break (_, value) -> (
      match value with
      | Some e -> "(break " ^ dump_expr e ^ ")"
      | None -> "(break)")
  | Continue _ -> "(continue)"
  | Match (scrutinee, arms) ->
      let arm (a : Ripe.Ast.arm) =
        " (" ^ dump_pattern a.pat ^ " "
        ^ dump_block a.arm_body.Ripe.Ast.value
        ^ ")"
      in
      "(match " ^ dump_expr scrutinee
      ^ String.concat "" (List.map arm arms)
      ^ ")"
  | PairAssign (ft, st, fv, sv) ->
      "(pair " ^ String.concat " " (List.map dump_expr [ ft; st; fv; sv ]) ^ ")"

and dump_pattern (p : Ripe.Ast.pattern) : string =
  match p.pdesc with
  | PatValue e -> dump_expr e
  | PatWild -> "_"
  | PatBind name -> Ripe.Interner.text name

and dump_block (body : Ripe.Ast.block) : string =
  let dump_item (item : Ripe.Ast.block_item) =
    match item with
    | Expr e -> dump_expr e
    | Decl (LocalStruct sd) ->
        "(local struct " ^ Ripe.Interner.text sd.struct_name ^ ")"
    | Decl (LocalTypeAlias td) ->
        "(local type " ^ Ripe.Interner.text td.alias_name ^ ")"
    | Decl (LocalNewtype td) ->
        "(local newtype " ^ Ripe.Interner.text td.alias_name ^ ")"
    | Decl (LocalFunc fd) ->
        "(local func "
        ^ Ripe.Interner.text fd.func_name
        ^ " " ^ dump_block fd.body ^ ")"
    | Decl (LocalEnum ed) ->
        "(local enum " ^ Ripe.Interner.text ed.enum_name ^ ")"
  in
  "(block " ^ String.concat " " (List.map dump_item body) ^ ")"

let dump_tokens src =
  let st = Ripe.Lexer.make_state 0 in
  let lexbuf = Ripe.Lexer.lexbuf_of_string src in
  let rec go () =
    let t, _, _ = Ripe.Lexer.read st lexbuf in
    print_endline (tok_str t);
    if t <> Ripe.Tokens.EOF then go ()
  in
  go ()
