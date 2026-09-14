(* SPDX-License-Identifier: Apache-2.0 *)

type severity = Error | Warning | Note | Help
type t

exception Errors of t list

val headline : t -> string
val primary : t -> Ast.span option
val detail_of : t -> string option

type sink
type ctx = { sm : Sourcemap.t; filename : string; color : bool }

val error : Ast.span -> ('a, unit, string, t) format4 -> 'a
val warning : Ast.span -> string -> t
val global_error : string -> t
val label : ('a, unit, string, t -> t) format4 -> 'a
val found : string -> t -> t
val secondary : Ast.span -> string -> t -> t
val detail : string -> t -> t
val help : string -> t -> t
val sink : unit -> sink
val emit : sink -> t -> unit
val has_errors : sink -> bool
val drain : sink -> t list
val take : sink -> t list
val severity_label : bool -> severity -> string
val render_with : (int -> ctx) -> ctx -> t -> string
val render : ctx -> t -> string
val internal : ?span:Ast.span -> string -> t
val ice : ?span:Ast.span -> string -> 'a
