(* SPDX-License-Identifier: GPL-2.0-only *)

let parse_module ?(file = 0) src =
  let st = Ripe.Lexer.make_state file in
  let lexbuf = Ripe.Lexer.lexbuf_of_string src in
  fst
    (Diag.run_stage (fun diags ->
         Ripe.Parser.parse ~diags (Ripe.Lexer.read st) lexbuf))

let parse ?(file = 0) src = (parse_module ~file src).decls

let front_src module_id src =
  let st = Ripe.Lexer.make_state 0 in
  let lexbuf = Ripe.Lexer.lexbuf_of_string src in
  let diags = Ripe.Diagnostic.sink () in
  let module_ = Ripe.Parser.parse ~diags (Ripe.Lexer.read st) lexbuf in
  let decls = module_.decls in
  let uses = Ripe.Resolve.resolve ~diags ~module_id decls in
  (decls, uses, diags)

let resolve_src module_id src =
  let decls, uses, diags = front_src module_id src in
  let uses, _ = Diag.finish diags uses in
  (decls, uses)

let load_program (files : (string * string) list) =
  let read_file name =
    match List.assoc_opt name files with
    | Some src -> src
    | None -> raise (Sys_error name)
  in
  let list_dir name = raise (Sys_error name) in
  let diags = Ripe.Diagnostic.sink () in
  let program =
    Ripe.Program.load ~diags ~read_file ~list_dir ~root_filename:"main.rp" ()
  in
  (Ripe.Resolve.resolve_program ~diags program, diags)

let run_program files =
  let headline (d : Ripe.Diagnostic.t) =
    print_endline d.Ripe.Diagnostic.headline
  in
  try
    let resolved, diags = load_program files in
    let checked =
      Ripe.Typechecker.typecheck ~diags resolved.Ripe.Resolve.uses
        resolved.Ripe.Resolve.decls
    in
    let _, warns = Diag.finish diags checked in
    List.iter headline warns;
    print_endline "ok"
  with Ripe.Diagnostic.Errors ds -> List.iter headline ds

(* the front of the pipeline every runner shares *)
let check_src src =
  let decls, uses, diags = front_src 0 src in
  Diag.finish diags (Ripe.Typechecker.typecheck ~diags uses decls)

let mir_src src =
  let program = Ripe.Mir_build.build (fst (check_src src)) in
  Ripe.Mir_verify.verify program;
  program

let source_of_src src _ = ("<test>", Ripe.Source_map.create ~base:0 src)

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
  let status = Sys.command cmd in
  if status <> 0 then begin
    let ic = open_in err in
    (try
       while true do
         print_endline (Span_utils.replace (input_line ic) ssa "<il>")
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
  let wrapped = "func _f() { return " ^ src ^ " }" in
  try
    match parse wrapped with
    | [ Ripe.Ast.Func { body = [ Expr { desc = Return (Some e); _ } ]; _ } ] ->
        print_endline (Dump.dump_expr e)
    | _ -> print_endline "<parse_expr: unexpected shape>"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render wrapped) diags

let run_src src =
  try
    let _, warns = check_src src in
    List.iter (Diag.render src) warns;
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_codegen src =
  try
    let il =
      Ripe.Codegen_qbe.emit_mir ~source_of:(source_of_src src) (mir_src src)
    in
    print_string il;
    check_qbe il
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_codegen_ok src =
  try
    let il =
      Ripe.Codegen_qbe.emit_mir ~source_of:(source_of_src src) (mir_src src)
    in
    check_qbe il;
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_codegen_contains src fragments =
  try
    let il =
      Ripe.Codegen_qbe.emit_mir ~source_of:(source_of_src src) (mir_src src)
    in
    List.iter
      (fun fragment ->
        let present =
          try
            ignore (Str.search_forward (Str.regexp_string fragment) il 0);
            true
          with Not_found -> false
        in
        Printf.printf "%s: %b\n" fragment present)
      fragments;
    check_qbe il
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_mir src = print_string (Ripe.Mir_dump.program (mir_src src))
