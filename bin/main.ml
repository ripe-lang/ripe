(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe

let stage = ref Driver.Bin
let out = ref ""
let file = ref ""
let emit_stages = "tokens, ast, tast, check, qbe, asm"

(* FIXME(0844): I'm still unsure if I have to have ripec: in front. Also, I should add colors. *)
let die msg =
  Printf.eprintf "ripec: error: %s\n" msg;
  exit 2

let stage_of_string = function
  | "tokens" -> Driver.Tokens
  | "ast" -> Driver.Ast
  | "tast" -> Driver.Tast
  | "check" -> Driver.Check
  | "qbe" -> Driver.Qbe
  | "asm" -> Driver.Asm
  | s ->
      die
        (Printf.sprintf "unknown emit stage `%s`, expected one of: %s" s
           emit_stages)

let usage_msg =
  Printf.sprintf
    "The Ripe compiler\n\n\
     USAGE:\n\
    \  ripec [OPTIONS] <FILE>\n\n\
     OPTIONS:\n\
    \  -emit <STAGE>       Stop compilation after a given stage\n\
    \                      Supported stages: %s\n\
    \  -o <FILE>           Write the compiler output to <FILE>\n\
    \  -h, --help          Display this list of options\n\n\
     EXAMPLES:\n\
    \  ripec source.rp\n\
    \  ripec -emit tast source.rp\n\
    \  ripec -o output.s source.rp"
    emit_stages

let print_help_and_exit () =
  print_endline usage_msg;
  exit 0

let speclist =
  [
    ("-emit", Arg.String (fun s -> stage := stage_of_string s), "");
    ("-o", Arg.Set_string out, "");
    ("-h", Arg.Unit print_help_and_exit, "");
    ("-help", Arg.Unit print_help_and_exit, "");
    ("--help", Arg.Unit print_help_and_exit, "");
  ]

let () =
  try
    Arg.parse_argv ~current:(ref 0) Sys.argv speclist (fun f -> file := f) "";
    if !file = "" then print_help_and_exit ()
    else Driver.compile ~stage:!stage ~out:!out ~filename:!file
  with
  | Arg.Help _ -> print_help_and_exit ()
  | Arg.Bad msg ->
      (* FIXME(e569): This code path is messing things up *)
      let clean_msg = String.init (String.length msg - 1) (fun i -> msg.[i]) in
      die clean_msg
