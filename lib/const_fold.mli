(* SPDX-License-Identifier: GPL-2.0-only *)

val run :
  emit:(Diagnostic.t -> unit) ->
  force_const:(Ast.span -> Symbol.key -> unit) ->
  local_value:(Symbol.t -> Const_eval.const_num option) ->
  global_value:(Symbol.t -> Const_eval.const_num option) ->
  fold_num:(Typed_ast.texpr -> Const_eval.const_num) ->
  Typed_ast.tdecl list ->
  Typed_ast.tdecl list
