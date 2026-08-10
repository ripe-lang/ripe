(* SPDX-License-Identifier: GPL-2.0-only *)

type severity = Error | Warning | Note | Help
type span_label = { span : Ast.span; message : string }

type t = {
  severity : severity;
  headline : string;
  primary : Ast.span option;
  primary_label : string option;
  labels : span_label list;
  notes : t list;
  detail : string option;
  suggestion : string option;
}

exception Errors of t list

type sink = t list ref
type ctx = { sm : Source_map.t; filename : string; color : bool }

val error : string -> t
val warning : string -> t
val note : string -> t
val at : Ast.span -> t -> t
val label : string -> t -> t
val secondary : Ast.span -> string -> t -> t
val add_note : t -> t -> t
val detail : string -> t -> t
val help : string -> t -> t
val error_at : Ast.span -> string -> t
val sink : unit -> sink
val emit : sink -> t -> unit
val emit_error_at : sink -> Ast.span -> string -> unit
val emit_warn_at : sink -> Ast.span -> string -> unit
val has_errors : sink -> bool
val drain : sink -> t list
val take : sink -> t list
val severity_label : bool -> severity -> string
val render_with : (int -> ctx) -> ctx -> t -> string
val render : ctx -> t -> string
val type_mismatch : Ast.span -> expected:string -> found:string -> t
val undefined_name : Ast.span -> string -> t
val with_type : Ast.span -> string -> string -> t
val redefinition : Ast.span -> prev:Ast.span -> t
val arity : Ast.span -> expected:string -> found:int -> t
val unsupported_abi : Ast.span -> t
val int_out_of_range : Ast.span -> ty:string -> t
val bad_operand : Ast.span -> op:string -> ty:string -> t
val opaque_operation : Ast.span -> string -> t
val expected_expression : Ast.span -> t
val expected_type : Ast.span -> t
val internal : ?span:Ast.span -> string -> t
val ice : ?span:Ast.span -> string -> 'a
