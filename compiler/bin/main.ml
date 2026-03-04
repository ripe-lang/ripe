let dump_ast = ref false
let do_typecheck = ref false
let file = ref ""

let spec =
  [
    ("-dump-ast", Arg.Set dump_ast, "Dump parsed AST");
    ("-typecheck", Arg.Set do_typecheck, "Run the typechecker");
  ]

let parse_file filename =
  (* TODO: make this exception safe *)
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let decls = Ripe.Parser.program Ripe.Lexer.read lexbuf in
  close_in ic;

  if !dump_ast then
    List.iter (fun d -> print_endline (Ripe.Ast.decl_to_string d)) decls;

  try
    Ripe.Typechecker.typecheck decls;
    print_endline "typecheck: ok"
  with Ripe.Typechecker.TypeError msg ->
    Printf.eprintf "typecheck error: %s\n" msg;
    exit 1

let () =
  (* TODO: Update usage text for options *)
  Arg.parse spec (fun f -> file := f) "Usage: ripec file.rp>";
  if !file = "" then Arg.usage spec "Usage: ripec <file.rp>"
  else parse_file !file