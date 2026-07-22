(* SPDX-License-Identifier: GPL-2.0-only *)

let parse src =
  let st = Ripe.Lexer.make_state () in
  let lexbuf = Lexing.from_string src in
  Ripe.Parser.parse (Ripe.Lexer.read st) lexbuf

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

let dump_tokens src =
  let st = Ripe.Lexer.make_state () in
  let lexbuf = Lexing.from_string src in
  let rec go () =
    let t, _ = Ripe.Lexer.read st lexbuf in
    print_endline (tok_str t);
    if t <> Ripe.Tokens.EOF then go ()
  in
  go ()

let ctx src =
  {
    Ripe.Diagnostic.sm = Ripe.Source_map.create src;
    filename = "<test>";
    color = false;
  }

let run_src src =
  try
    let decls = parse src in
    let uses = Ripe.Resolve.resolve decls in
    let _, warns = Ripe.Typechecker.typecheck uses decls in
    List.iter (fun d -> print_string (Ripe.Diagnostic.render (ctx src) d)) warns;
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags ->
    List.iter (fun d -> print_string (Ripe.Diagnostic.render (ctx src) d)) diags

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

let qbe = match Sys.getenv_opt "QBE" with Some p -> p | None -> "qbe"

(* feed the il through qbe so malformed output fails the test *)
let check_qbe il =
  let ssa = Filename.temp_file "ripe_test" ".ssa" in
  let err = Filename.temp_file "ripe_test" ".err" in
  let oc = open_out ssa in
  output_string oc il;
  close_out oc;
  let cmd =
    Printf.sprintf "%s -o /dev/null %s 2> %s" (Filename.quote qbe)
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

let run_codegen src =
  try
    let decls = parse src in
    let uses = Ripe.Resolve.resolve decls in
    let tdecls, _ = Ripe.Typechecker.typecheck uses decls in
    let cdecls = Ripe.Lower.lower tdecls in
    let il = Ripe.Codegen_qbe.emit_qbe cdecls in
    print_string il;
    check_qbe il
  with Ripe.Diagnostic.Errors diags ->
    List.iter (fun d -> print_string (Ripe.Diagnostic.render (ctx src) d)) diags

let expect_errors f =
  try
    f ();
    print_endline "<no error>"
  with Ripe.Diagnostic.Errors diags ->
    List.iter (fun d -> print_string (Ripe.Diagnostic.render (ctx "") d)) diags

let parse_only src =
  try List.iter (fun d -> print_endline (Ripe.Ast.show_decl d)) (parse src)
  with Ripe.Diagnostic.Errors diags ->
    List.iter (fun d -> print_string (Ripe.Diagnostic.render (ctx src) d)) diags

let show_unop = function
  | Ripe.Ast.Neg -> "neg"
  | Not -> "!"
  | BitNot -> "~"
  | Deref -> "deref"
  | AddressOf -> "addr"

(* compact s-expr expression dumper, no spans *)
let rec dump_typ (t : Ripe.Ast.typ) =
  match t.tdesc with
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
  | Int (n, suf) -> Int64.to_string n ^ Option.value ~default:"" suf
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Null -> "null"
  | Char c -> "'" ^ String.make 1 c ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | Ident s -> s
  | Call (callee, args) ->
      "(call " ^ dump_expr callee
      ^ String.concat "" (List.map (fun a -> " " ^ dump_expr a) args)
      ^ ")"
  | BinOp (op, l, r) ->
      "(" ^ Ripe.Ast.show_binop_sym op ^ " " ^ dump_expr l ^ " " ^ dump_expr r
      ^ ")"
  | UnOp (op, e) -> "(" ^ show_unop op ^ " " ^ dump_expr e ^ ")"
  | Range (l, r) -> "(.. " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | RangeInclusive (l, r) -> "(..= " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | FieldAccess (e, f) -> "(. " ^ dump_expr e ^ " " ^ f ^ ")"
  | Cast (e, t) -> "(as " ^ dump_expr e ^ " " ^ dump_typ t ^ ")"
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
  | BlockExpr (stmts, value) ->
      "(block "
      ^ string_of_int (List.length stmts)
      ^ " " ^ dump_expr value ^ ")"

(* wrap src in `return ...` so callers can write bare expressions *)
let parse_expr src =
  let wrapped = "func _f() { return " ^ src ^ " }" in
  try
    match parse wrapped with
    | [ Ripe.Ast.Func { body = [ { sdesc = Return (Some e); _ } ]; _ } ] ->
        print_endline (dump_expr e)
    | _ -> print_endline "<parse_expr: unexpected shape>"
  with Ripe.Diagnostic.Errors diags ->
    List.iter
      (fun d -> print_string (Ripe.Diagnostic.render (ctx wrapped) d))
      diags
