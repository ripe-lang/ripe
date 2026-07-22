(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast

(* The binder/use is keyed by its span. *)
type t = { syms : (Ast.span, Symbol.t) Hashtbl.t }

type state = {
  out : t;
  globals : (string, Symbol.t) Hashtbl.t;
  types : (string, Symbol.t) Hashtbl.t;
  mutable scopes : (string, Symbol.t) Hashtbl.t list;
  mutable next_id : Symbol.id;
  diags : Diagnostic.sink;
}

(* The `--emit resolve` output. *)
let dump (r : t) : string =
  Hashtbl.to_seq r.syms |> List.of_seq
  |> List.sort (fun ((a : Ast.span), _) ((b : Ast.span), _) ->
      compare (a.lo, a.hi) (b.lo, b.hi))
  |> List.map (fun ((sp : Ast.span), (s : Symbol.t)) ->
      Printf.sprintf "(%d,%d) -> #%d %s %s\n" sp.lo sp.hi s.id
        (Symbol.show_kind s.kind) s.name)
  |> String.concat ""

let sym_at (r : t) (span : Ast.span) : Symbol.t =
  match Hashtbl.find_opt r.syms span with
  | Some s -> s
  | None -> Error.ice ~span "no symbol resolved here"

let mint (st : state) (kind : Symbol.kind) (name : string) (span : Ast.span) :
    Symbol.t =
  let id = st.next_id in
  st.next_id <- id + 1;
  let sym = { Symbol.id; name; kind; span } in
  Hashtbl.replace st.out.syms span sym;
  sym

let push_scope (st : state) = st.scopes <- Hashtbl.create 8 :: st.scopes
let pop_scope (st : state) = st.scopes <- List.tl st.scopes

(* The innermost scope holds params with the top level body binders. *)
let declare_local (st : state) kind name span : unit =
  let sym = mint st kind name span in
  match st.scopes with
  | scope :: _ -> Hashtbl.replace scope name sym
  | [] -> assert false

let declare_global (st : state) kind name span : unit =
  match Hashtbl.find_opt st.globals name with
  | Some prev ->
      Diagnostic.emit st.diags
        (Error.redefinition span ~prev:prev.Symbol.span name)
  | None ->
      let sym = mint st kind name span in
      Hashtbl.replace st.globals name sym

let declare_type (st : state) name span : unit =
  if not (Hashtbl.mem st.types name) then
    Hashtbl.replace st.types name (mint st Symbol.Type name span)

let use_type (st : state) name span : unit =
  if not (List.mem_assoc name Types.builtin_tys) then
    match Hashtbl.find_opt st.types name with
    | Some sym -> Hashtbl.replace st.out.syms span sym
    | None -> Diagnostic.emit st.diags (Error.undefined_name span "type" name)

let lookup (st : state) (name : string) : Symbol.t option =
  match List.find_map (fun scope -> Hashtbl.find_opt scope name) st.scopes with
  | Some s -> Some s
  | None -> Hashtbl.find_opt st.globals name

let use (st : state) ~(what : string) name span : unit =
  match lookup st name with
  | Some sym -> Hashtbl.replace st.out.syms span sym
  | None -> Diagnostic.emit st.diags (Error.undefined_name span what name)

let rec resolve_expr (st : state) (e : expr) : unit =
  match e.desc with
  | Ident name -> use st ~what:"variable" name e.span
  | Call ({ desc = Ident name; span }, args) ->
      use st ~what:"function" name span;
      List.iter (resolve_expr st) args
  | Call (callee, args) ->
      resolve_expr st callee;
      List.iter (resolve_expr st) args
  | BinOp (_, l, r) ->
      resolve_expr st l;
      resolve_expr st r
  | UnOp (_, inner) -> resolve_expr st inner
  | Range (l, r) | RangeInclusive (l, r) ->
      resolve_expr st l;
      resolve_expr st r
  | FieldAccess (inner, _) -> resolve_expr st inner
  | Cast (inner, ty, _) ->
      resolve_expr st inner;
      resolve_typ st ty
  | SizeOf ty -> resolve_typ st ty
  | Index (base, idx) ->
      resolve_expr st base;
      resolve_expr st idx
  | ArrayLit elems -> List.iter (resolve_expr st) elems
  | StructLit (_, _, fields) ->
      List.iter (fun (_, _, e) -> resolve_expr st e) fields
  | Int _ | Float _ | Bool _ | Null | Char _ | String _ | Undefined -> ()

(* an array size expression may name constants *)
and resolve_typ (st : state) (t : typ) : unit =
  match t.tdesc with
  | Named "opaque" -> ()
  | Named name -> use_type st name t.span
  | Pointer t | Slice t -> resolve_typ st t
  | Array (e, t) ->
      resolve_expr st e;
      resolve_typ st t
  | FuncPtr (ps, ret) ->
      List.iter (resolve_typ st) ps;
      Option.iter (resolve_typ st) ret

let rec resolve_stmt (st : state) (s : stmt) : unit =
  match s.sdesc with
  | Binding (kind, name, nspan, ann, e) ->
      Option.iter (resolve_typ st) ann;
      Option.iter (resolve_expr st) e;
      declare_local st (Symbol.Local kind) name nspan
  | Return e -> Option.iter (resolve_expr st) e
  | Expr e -> resolve_expr st e
  | If (branches, else_body) ->
      List.iter
        (fun (cond, body) ->
          resolve_expr st cond;
          resolve_block st body)
        branches;
      resolve_block st else_body
  | While (cond, body) ->
      resolve_expr st cond;
      resolve_block st body
  | For (name, nspan, iter, body) ->
      resolve_expr st iter;
      push_scope st;
      declare_local st Symbol.ForVar name nspan;
      List.iter (resolve_stmt st) body;
      pop_scope st
  | Block body -> resolve_block st body
  | Break | Continue -> ()

and resolve_block (st : state) (body : stmt list) : unit =
  push_scope st;
  List.iter (resolve_stmt st) body;
  pop_scope st

(* Body binders can redeclare freely but params can't repeat. *)
let declare_param (st : state) (p : param) : unit =
  let scope = List.hd st.scopes in
  match Hashtbl.find_opt scope p.name with
  | Some prev ->
      Diagnostic.emit st.diags
        (Error.redefinition p.span ~prev:prev.Symbol.span p.name)
  | None -> declare_local st Symbol.Param p.name p.span

let resolve_func (st : state) (fd : func_def) : unit =
  push_scope st;
  List.iter
    (fun (p : param) ->
      resolve_typ st p.typ;
      declare_param st p)
    fd.params;
  Option.iter (resolve_typ st) fd.ret;
  List.iter (resolve_stmt st) fd.body;
  pop_scope st

let resolve (decls : decl list) : t =
  let st =
    {
      out = { syms = Hashtbl.create 256 };
      globals = Hashtbl.create 64;
      types = Hashtbl.create 64;
      scopes = [];
      next_id = 0;
      diags = Diagnostic.sink ();
    }
  in
  (* Declare every top level name first so bodies and initializers can
     forward reference. *)
  List.iter
    (function
      | Func fd -> declare_global st Symbol.Func fd.name fd.span
      | Extern fd -> declare_global st Symbol.Extern fd.name fd.span
      | Global gd -> declare_global st Symbol.Global gd.name gd.span
      | Struct sd -> declare_type st sd.name sd.span
      | TypeAlias td | Newtype td -> declare_type st td.name td.span)
    decls;
  List.iter
    (function
      | Func fd | Extern fd -> resolve_func st fd
      | Global gd ->
          resolve_typ st gd.typ;
          Option.iter (resolve_expr st) gd.init
      | Struct sd ->
          List.iter (fun (f : field) -> resolve_typ st f.typ) sd.fields
      | TypeAlias td | Newtype td -> resolve_typ st td.typ)
    decls;
  let all = Diagnostic.drain st.diags in
  if List.exists (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error) all
  then raise (Diagnostic.Errors all)
  else st.out
