(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe
open Climate

let emit_stages =
  [
    (Driver.Tokens, "the lexer token stream");
    (Driver.Ast, "the untyped syntax tree");
    (Driver.Resolve, "the resolved symbol table");
    (Driver.Tast, "the typed syntax tree");
    (Driver.Check, "the typecheck result");
    (Driver.Mir, "the middle intermediate representation");
    (Driver.Qbe, "the QBE intermediate representation");
    (Driver.Asm, "the target assembly");
    (Driver.Obj, "the relocatable object file");
  ]

let stage_help =
  String.concat "\n"
    (List.map
       (fun (stage, desc) ->
         Printf.sprintf "                          %-6s  %s"
           (Driver.stage_name stage) desc)
       emit_stages)

let backends = [ Driver.Backend.Qbe; Driver.Backend.X86 ]
let backend_names = String.concat ", " (List.map Driver.Backend.name backends)

let usage_msg =
  Printf.sprintf
    "The Ripe compiler\n\n\
     Usage: ripec [OPTIONS] <FILE>\n\n\
     Options:\n\
    \  -e, --emit <STAGE>    Stop compilation after <STAGE> and print its \
     output:\n\
     %s\n\
    \  -b, --backend <NAME>  Select the code generator: %s (default qbe)\n\
    \  -o, --output <FILE>   Write the compiler output to <FILE>\n\
    \  -l, --library <NAME>  Link with the named library\n\
    \  -I, --import-path <DIR>\n\
    \                        Add a directory to the import search path\n\
    \  --stats               Print compiler timing and tool commands\n\
    \  -h, --help            Display this list of options\n\n\
     Examples:\n\
    \  ripec source.rp\n\
    \  ripec --emit tast source.rp\n\
    \  ripec -o output.s source.rp"
    stage_help backend_names

let emit_conv =
  Arg_parser.enum ~default_value_name:"STAGE"
    (List.map (fun (stage, _) -> (Driver.stage_name stage, stage)) emit_stages)

let backend_conv =
  Arg_parser.enum ~default_value_name:"NAME"
    (List.map (fun backend -> (Driver.Backend.name backend, backend)) backends)

let command =
  let open Arg_parser in
  let+ stage = named_opt [ "e"; "emit" ] emit_conv
  and+ backend =
    named_with_default [ "b"; "backend" ] backend_conv
      ~default:Driver.Backend.Qbe
  and+ out =
    named_with_default [ "o"; "output" ] file ~default:"" ~value_name:"FILE"
  and+ libraries = named_multi [ "l"; "library" ] string ~value_name:"NAME"
  and+ import_paths =
    named_multi [ "I"; "import-path" ] string ~value_name:"DIR"
  and+ stats = flag [ "stats" ]
  and+ filename = pos_req 0 file ~value_name:"FILE" in
  let stage = Option.value stage ~default:Driver.Bin in
  let search_roots =
    import_paths @ Config.search_roots ~root_filename:filename ()
  in
  Driver.compile ~stage ~backend ~out ~libraries ~search_roots ~stats ~filename

let is_help arg = arg = "-h" || arg = "--help"

let () =
  let args = match Array.to_list Sys.argv with _ :: rest -> rest | [] -> [] in
  if List.is_empty args || List.exists is_help args then (
    print_endline usage_msg;
    exit 0)
  else
    Command.eval ~program_name:(Literal "ripec") ~help_style:Help_style.plain
      (Command.singleton command)
      args
