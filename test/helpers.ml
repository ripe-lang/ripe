(* SPDX-License-Identifier: GPL-2.0-only *)

module C = Ripe.Core

let parse_module ?(file = 0) src =
  let st = Ripe.Lexer.make_state file in
  let lexbuf = Lexing.from_string src in
  Ripe.Parser.parse (Ripe.Lexer.read st) lexbuf

let parse ?(file = 0) src = (parse_module ~file src).decls

let substring_offset src sub =
  let n = String.length sub and m = String.length src in
  let rec go i =
    if i + n > m then failwith (Printf.sprintf "substring not found: %S" sub)
    else if String.sub src i n = sub then i
    else go (i + 1)
  in
  go 0

let span src sub =
  let lo = substring_offset src sub in
  Ripe.Span.make 0 lo (lo + String.length sub)

let point src sub =
  let offset = substring_offset src sub in
  Ripe.Span.make 0 offset offset

let replace s old rep =
  let olen = String.length old in
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    if !i + olen <= String.length s && String.sub s !i olen = old then begin
      Buffer.add_string b rep;
      i := !i + olen
    end
    else begin
      Buffer.add_char b s.[!i];
      incr i
    end
  done;
  Buffer.contents b

let ctx src =
  {
    Ripe.Diagnostic.sm = Ripe.Source_map.create src;
    filename = "<test>";
    color = false;
  }

let render src d = print_string (Ripe.Diagnostic.render (ctx src) d)

let resolve_src module_id src =
  let decls = parse src in
  (decls, Ripe.Resolve.resolve ~module_id decls)

(* the front of the pipeline every runner shares *)
let check_src src =
  let decls, uses = resolve_src 0 src in
  Ripe.Typechecker.typecheck uses decls

let lower_src src = Ripe.Lower.lower (fst (check_src src))

(* feed the il through qbe so malformed output fails the test *)
let check_qbe il =
  let ssa = Filename.temp_file "ripe_test" ".ssa" in
  let err = Filename.temp_file "ripe_test" ".err" in
  let oc = open_out ssa in
  output_string oc il;
  close_out oc;
  let cmd =
    Printf.sprintf "%s -o /dev/null %s 2> %s"
      (Filename.quote Ripe.Config.qbe)
      (Filename.quote ssa) (Filename.quote err)
  in
  if Sys.command cmd <> 0 then begin
    let ic = open_in err in
    (try
       while true do
         print_endline (replace (input_line ic) ssa "<il>")
       done
     with End_of_file -> ());
    close_in ic
  end;
  Sys.remove ssa;
  Sys.remove err

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
  | Named n -> n
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
  | StructLit (name, _, fields) ->
      "(struct " ^ name
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

let run_parse src =
  try
    ignore (parse src);
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (render src) diags

(* wrap src in `return ...` so callers can write bare expressions *)
let parse_expr src =
  let wrapped = "func _f() { return " ^ src ^ " }" in
  try
    match parse wrapped with
    | [ Ripe.Ast.Func { body = [ { desc = Return (Some e); _ } ]; _ } ] ->
        print_endline (dump_expr e)
    | _ -> print_endline "<parse_expr: unexpected shape>"
  with Ripe.Diagnostic.Errors diags -> List.iter (render wrapped) diags

let run_src src =
  try
    let _, warns = check_src src in
    List.iter (render src) warns;
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (render src) diags

let run_codegen src =
  try
    let il = Ripe.Codegen_qbe.emit_qbe (lower_src src) in
    print_string il;
    check_qbe il
  with Ripe.Diagnostic.Errors diags -> List.iter (render src) diags

let run_lower src =
  List.iter
    (function
      | C.CFunc fd -> print_endline (fd.C.name ^ " " ^ dump_cstmts fd.C.body)
      | _ -> ())
    (lower_src src)

let expect_errors f =
  try
    f ();
    print_endline "<no error>"
  with Ripe.Diagnostic.Errors diags ->
    List.iter (fun d -> print_string (Ripe.Diagnostic.render (ctx "") d)) diags
