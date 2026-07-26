(* SPDX-License-Identifier: GPL-2.0-only *)

(*
  One Ripe source file is one module with its source info, parsed code, and
  depends.

  main.rp -> module 0 { source, parsed code, math -> module 1 }
  math.rp -> module 1 { source, parsed code }
  program -> root module 0 + every reached module

  A depend keeps the written import and the ID of the module it found so repeated
  imports can refer to the same module.
*)

(* This stays with the module because diagnostics need the original file *)
type source = {
  file_id : Span.file_id;
  filename : string;
  source_map : Source_map.t;
}

(* This uses an ID so several imports can point to the same module *)
type dependency = { import : Ast.import; target : Symbol.module_id }

type module_ = {
  module_id : Symbol.module_id;
  path : string list;
  source : source;
  ast : Ast.module_;
  dependencies : dependency list;
}

type t = { root : module_; modules : module_ array }
