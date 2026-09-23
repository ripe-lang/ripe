(* SPDX-License-Identifier: Apache-2.0 *)

type t
type sink
type ctx = { sm : Sourcemap.t; filename : string; color : bool }
type severity = Error | Warning | Note | Help

exception Errors of t list

val detail : string -> t -> t
val detail_of : t -> string option
val drain : sink -> t list
val emit : sink -> t -> unit
val error : Ast.span -> ('a, unit, string, t) format4 -> 'a
val error_no_span : string -> t
val found : string -> t -> t
val has_errors : sink -> bool
val headline : t -> string
val help : string -> t -> t
val ice : ?span:Ast.span -> string -> 'a
val internal : ?span:Ast.span -> string -> t
val label : ('a, unit, string, t -> t) format4 -> 'a
val primary : t -> Ast.span option
val render : ctx -> t -> string
val render_with : (int -> ctx) -> ctx -> t -> string
val secondary : Ast.span -> string -> t -> t
val severity_label : bool -> severity -> string
val sink : unit -> sink
val take : sink -> t list
val warning : Ast.span -> string -> t
