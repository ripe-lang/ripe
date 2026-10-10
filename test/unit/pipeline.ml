(* SPDX-License-Identifier: Apache-2.0 *)

let parse_src src =
  let lexbuf = Ripe.Lexer.lexbuf_of_string src in
  try Ripe.Parser.parse Ripe.Lexer.read lexbuf with Ripe.Parser.Failed -> []

let parse src = fst (Diag.run_stage (fun () -> parse_src src))

let front_src src =
  Diag.fresh ();
  let decls = parse_src src in
  (decls, Ripe.Resolve.resolve decls)

let resolve_src src =
  let decls, uses = front_src src in
  (decls, fst (Diag.finish uses))

(* the front of the pipeline every runner shares *)
let check_src src =
  let decls, uses = front_src src in
  Diag.finish (Ripe.Sema.analyze uses decls)

let mir_src src =
  let tdecls = fst (check_src src) in
  let program = Ripe.Mir.build tdecls in
  Ripe.Mir.verify program;
  program

let emit_src src =
  Ripe.Codegenqbe.emit ~filename:"<test>"
    ~source_map:(Ripe.Sourcemap.create src)
    (mir_src src)

(* feed the il through qbe so malformed output fails the test *)
let check_qbe il =
  let ssa = Filename.temp_file "ripe_test" ".ssa" in
  let err = Filename.temp_file "ripe_test" ".err" in
  let oc = open_out ssa in
  output_string oc il;
  close_out oc;
  let cmd =
    Printf.sprintf "%s -o /dev/null %s 2> %s"
      (Filename.quote (Ripe.Config.qbe ()))
      (Filename.quote ssa) (Filename.quote err)
  in
  let status = Sys.command cmd in
  if status <> 0 then begin
    let ic = open_in err in
    (try
       while true do
         print_endline (Spanutils.replace (input_line ic) ssa "<il>")
       done
     with End_of_file -> ());
    close_in ic;
    Sys.remove ssa;
    Sys.remove err;
    failwith "qbe rejected generated MIR"
  end;
  Sys.remove ssa;
  Sys.remove err

let run_parse src =
  try
    ignore (parse src);
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

(* wrap src in `return ...` so callers can write bare expressions *)
let parse_expr src =
  let wrapped = "_f :: fn() { return " ^ src ^ " };" in
  try
    match parse wrapped with
    | [ Ripe.Ast.Func { body = [ Expr { desc = Return (Some e); _ } ]; _ } ] ->
        print_endline (Dump.dump_expr e)
    | _ -> print_endline "<parse_expr: unexpected shape>"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render wrapped) diags

let parse_body src =
  try
    match parse src with
    | [ Ripe.Ast.Func fd ] -> print_endline (Dump.dump_block fd.body)
    | _ -> print_endline "<parse_body: unexpected shape>"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_src src =
  try
    let _, warns = check_src src in
    List.iter (Diag.render src) warns;
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_codegen src =
  try
    let il = emit_src src in
    print_string il;
    check_qbe il
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_codegen_ok src =
  try
    let il = emit_src src in
    check_qbe il;
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_mir src = print_string (Ripe.Mir.dump (mir_src src))
