(* SPDX-License-Identifier: Apache-2.0 *)

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

let read_from files name =
  match List.assoc_opt name files with
  | Some src -> src
  | None -> raise (Sys_error name)

let list_from files dir =
  let prefix = if String.is_empty dir then "" else dir ^ Filename.dir_sep in
  let strip name =
    String.sub name (String.length prefix)
      (String.length name - String.length prefix)
  in
  let entries =
    files |> List.map fst
    |> List.filter (String.starts_with ~prefix)
    |> List.map strip
    |> List.filter (fun name -> not (String.contains name '/'))
  in
  if List.is_empty entries then raise (Sys_error dir) else entries

let load_tree ?(search_roots = []) (files : (string * string) list) =
  let diags = Ripe.Diagnostic.sink () in
  let program =
    Ripe.Program.load ~diags ~read_file:(read_from files)
      ~list_dir:(list_from files) ~search_roots ~root_filename:"main.rp" ()
  in
  (program, diags)

let load_program ?search_roots (files : (string * string) list) =
  let program, diags = load_tree ?search_roots files in
  (Ripe.Resolve.resolve_program ~diags program, diags)

let run_resolve_program ?search_roots files =
  let program, diags = load_tree ?search_roots files in
  let resolved = Ripe.Resolve.resolve_program ~diags program in
  match Diag.finish diags resolved with
  | _ -> print_endline "ok"
  | exception Ripe.Diagnostic.Errors ds -> List.iter (Diag.render_in program) ds

let run_program files =
  let headline (d : Ripe.Diagnostic.t) =
    print_endline (Ripe.Diagnostic.headline d)
  in
  try
    let resolved, diags = load_program files in
    let checked =
      Ripe.Sema.analyze ~diags resolved.Ripe.Resolve.uses
        resolved.Ripe.Resolve.decls
    in
    let _, warns = Diag.finish diags checked in
    List.iter headline warns;
    print_endline "ok"
  with Ripe.Diagnostic.Errors ds -> List.iter headline ds

(* the front of the pipeline every runner shares *)
let check_src src =
  let decls, uses, diags = front_src 0 src in
  Diag.finish diags (Ripe.Sema.analyze ~diags uses decls)

let mir_src src =
  let tdecls = fst (check_src src) in
  let program = Ripe.Mir.build tdecls in
  match Ripe.Mir.verify program with
  | Ok () -> program
  | Error errors ->
      errors |> List.map Ripe.Mir.show_error |> String.concat "\n" |> failwith

let source_of_src src _ = ("<test>", Ripe.Sourcemap.create ~base:0 src)

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
  let wrapped = "func _f() { return " ^ src ^ " }" in
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
    let il =
      Ripe.Codegenqbe.emit ~source_of:(source_of_src src) (mir_src src)
    in
    print_string il;
    check_qbe il
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_codegen_ok src =
  try
    let il =
      Ripe.Codegenqbe.emit ~source_of:(source_of_src src) (mir_src src)
    in
    check_qbe il;
    print_endline "ok"
  with Ripe.Diagnostic.Errors diags -> List.iter (Diag.render src) diags

let run_mir src = print_string (Ripe.Mir.dump (mir_src src))
