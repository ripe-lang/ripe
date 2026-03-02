let dump_ast = ref false
let file = ref ""
let spec = [ ("-dump-ast", Arg.Set dump_ast, "Dump parsed AST") ]

let parse_file filename =
  (* TODO: make this exception safe *)
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let decls = Ripe.Parser.program Ripe.Lexer.read lexbuf in
  close_in ic;
  if !dump_ast then
    List.iter (fun d -> print_endline (Ripe.Ast.decl_to_string d)) decls

let () =
  Arg.parse spec (fun f -> file := f) "Usage: ripe <file.rp>";
  if !file = "" then Arg.usage spec "Usage: ripe <file.rp>"
  else parse_file !file
