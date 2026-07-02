(* SPDX-License-Identifier: GPL-2.0-only *)

let parse src =
  Ripe.Lexer.reset ();
  let lexbuf = Lexing.from_string src in
  Ripe.Parser.parse Ripe.Lexer.read lexbuf

let run_src src =
  match parse src with
  | decls -> (
      match Ripe.Typechecker.typecheck "<test>" src decls with
      | _ -> print_endline "ok"
      | exception Ripe.Typechecker.TypeErrors msgs ->
          List.iter (fun msg -> print_endline ("TypeError: " ^ msg)) msgs)
  | exception Ripe.Parser.ParseErrors diags ->
      List.iter (fun (_, msg) -> print_endline ("ParseError: " ^ msg)) diags

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

(* feed the il through qbe so malformed output fails the test *)
let check_qbe il =
  let ssa = Filename.temp_file "ripe_test" ".ssa" in
  let err = Filename.temp_file "ripe_test" ".err" in
  let oc = open_out ssa in
  output_string oc il;
  close_out oc;
  let cmd =
    Printf.sprintf "qbe -o /dev/null %s 2> %s" (Filename.quote ssa)
      (Filename.quote err)
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
  match parse src with
  | decls -> (
      match Ripe.Typechecker.typecheck "<test>" src decls with
      | tdecls ->
          let il = Ripe.Codegen.emit_qbe tdecls in
          print_string il;
          check_qbe il
      | exception Ripe.Typechecker.TypeErrors msgs ->
          List.iter (fun msg -> print_endline ("TypeError: " ^ msg)) msgs)
  | exception Ripe.Parser.ParseErrors diags ->
      List.iter (fun (_, msg) -> print_endline ("ParseError: " ^ msg)) diags

let parse_only src =
  match parse src with
  | decls -> List.iter (fun d -> print_endline (Ripe.Ast.show_decl d)) decls
  | exception Ripe.Parser.ParseErrors diags ->
      List.iter (fun (_, msg) -> print_endline ("ParseError: " ^ msg)) diags

(* compact s-expr expression dumper, no spans *)
let rec dump_typ (t : Ripe.Ast.typ) =
  match t.tdesc with
  | Named n -> n
  | Pointer p -> "*" ^ dump_typ p
  | Array (n, t) -> Printf.sprintf "[%d]%s" n (dump_typ t)
  | Slice t -> "[]" ^ dump_typ t
  | FuncPtr (params, ret) ->
      let ps = String.concat ", " (List.map dump_typ params) in
      let r = match ret with Some t -> " " ^ dump_typ t | None -> "" in
      "(" ^ ps ^ ")" ^ r

let show_unop = function
  | Ripe.Ast.Neg -> "neg"
  | Not -> "!"
  | BitNot -> "~"
  | Deref -> "deref"
  | AddressOf -> "addr"

let rec dump_expr (e : Ripe.Ast.expr) =
  match e.desc with
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Null -> "null"
  | Char c -> "'" ^ String.make 1 c ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | Ident s -> s
  | Call (n, args) ->
      "(call " ^ n
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
  | InterpString _ -> "<interp>"
  | ArrayLit elems ->
      "(array"
      ^ String.concat "" (List.map (fun e -> " " ^ dump_expr e) elems)
      ^ ")"
  | Index (base, idx) -> "(index " ^ dump_expr base ^ " " ^ dump_expr idx ^ ")"
  | Undefined -> "undefined"

(* wrap src in `return ...` so callers can write bare expressions *)
let parse_expr src =
  let wrapped = "func _f() { return " ^ src ^ " }" in
  match parse wrapped with
  | [ Ripe.Ast.Func { body = [ { sdesc = Return (Some e); _ } ]; _ } ] ->
      print_endline (dump_expr e)
  | _ -> print_endline "<parse_expr: unexpected shape>"
  | exception Ripe.Parser.ParseErrors diags ->
      List.iter (fun (_, msg) -> print_endline ("ParseError: " ^ msg)) diags
