(* SPDX-License-Identifier: Apache-2.0 *)

open Ast

(* A name is interned so a scope lookup is an int compare *)
module Names = Hashtbl.Make (struct
  type t = name

  let equal a b = a = b
  let hash t = t
end)

type namespace = Symbol.t Names.t

(* A type, a fn and a value share one set of names like Swift *)
type scope = { names : namespace; parent : scope option }

(* The shadowed table keeps what a callee would have found if a value hadn't taken the name *)
type t = {
  syms : Symbol.t Span.Table.t;
  shadowed : Symbol.t Span.Table.t;
  prelude : scope;
  mutable local_decls : decl list;
}

type state = {
  out : t;
  top : scope;
  scope : scope;
  value_boundary : scope option;
  next_id : Symbol.id ref;
}

let new_scope parent = { names = Names.create 16; parent }

(* A builtin sits in the outermost scope so a program can shadow it like any name *)
let make_output () =
  let prelude = new_scope None in
  List.iteri
    (fun i (name, _) ->
      (* We go negative so a builtin never grabs an id the program wants *)
      let id = (Symbol.unresolved_key :> int) - 1 - i in
      Names.replace prelude.names (Interner.intern name)
        {
          Symbol.id;
          name;
          link_name = name;
          kind = Symbol.Type;
          entry_point = false;
          span = dummy_span;
          name_span = dummy_span;
        })
    Types.builtins;
  {
    syms = Span.Table.create 16;
    shadowed = Span.Table.create 16;
    prelude;
    local_decls = [];
  }

(* This is the `--emit resolve` output *)
let dump r =
  let entries =
    Span.Table.to_seq r.syms |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map (fun (sp, s) ->
        Printf.sprintf "(%d,%d) -> #%d %s %s\n" (Span.lo sp) (Span.hi sp)
          s.Symbol.id
          (Symbol.show_kind s.Symbol.kind)
          s.Symbol.name)
    |> String.concat ""
  in
  "Resolver output\n\n\
   Each line maps a source byte range to its definition.\n\
   * (start,end): source byte range\n\
   * #id: declaration ID\n\
   * #-id: built in declaration\n\
   * kind name: resolved definition\n\n" ^ entries

let sym_at r span =
  match Span.Table.find_opt r.syms span with
  | Some s -> s
  | None -> Diagnostic.ice ~span "no symbol resolved here"

let sym_at_opt r span = Span.Table.find_opt r.syms span
let shadowed_at r span = Span.Table.find_opt r.shadowed span
let keyname_of s = Keyname.make (Symbol.key s) s.Symbol.name
let local_decls r = List.rev r.local_decls

let builtins r =
  List.map
    (fun (name, builtin) ->
      (Symbol.key (Names.find r.prelude.names (Interner.intern name)), builtin))
    Types.builtins

let main_name = Interner.intern "main"

let mint st kind name span link_name name_span =
  let id = !(st.next_id) in
  st.next_id := id + 1;
  let sym =
    {
      Symbol.id;
      name = Interner.text name;
      link_name;
      kind;
      entry_point = kind = Symbol.Func && name = main_name;
      span;
      name_span;
    }
  in
  Span.Table.replace st.out.syms span sym;
  sym

(* The inner state goes out of scope with the block so none can stay open *)
let enter_scope st = { st with scope = new_scope (Some st.scope) }

(* The innermost scope holds params and top level body binders *)
let declare_local st kind name span =
  (* A param or arm name can't repeat and a :: keeps its name for the whole block *)
  match Names.find_opt st.scope.names name with
  | Some prev
    when kind = Symbol.Param || kind = Symbol.MatchBind
         || not (Symbol.is_local prev.Symbol.kind) ->
      Diagnostic.emit
        (Diagnostic.error span "already defined"
        |> Diagnostic.secondary prev.Symbol.name_span "previous definition here"
        );
      ignore (mint st Symbol.Error name span (Interner.text name) span)
  | _ ->
      Names.replace st.scope.names name
        (mint st kind name span (Interner.text name) span)

let decl_link_name name = "_R" ^ string_of_int (String.length name) ^ name

let declare_in table st kind ident span link_name =
  match Names.find_opt table ident.value with
  | Some prev ->
      Diagnostic.emit
        (Diagnostic.error ident.span "already defined"
        |> Diagnostic.secondary prev.Symbol.name_span "previous definition here"
        );
      (* The repeat stays out of the scope but still gets a key so its body is checked *)
      ignore (mint st kind ident.value span link_name ident.span)
  | None ->
      Names.replace table ident.value
        (mint st kind ident.value span link_name ident.span)

(* A body's :: names go in first so a call can come before them *)
let declare_local_bound st b =
  let text = Interner.text b.bind_name.value in
  begin match bind_value b with
  | Some { edesc = Extern _; _ } ->
      declare_in st.scope.names st Symbol.Extern b.bind_name b.bind_span text
  | Some { edesc = Lambda _; _ } ->
      let link_name = Printf.sprintf "_Rlocal%d_%s" !(st.next_id) text in
      declare_in st.scope.names st Symbol.LocalFunc b.bind_name b.bind_span
        link_name
  | Some _ | None ->
      declare_in st.scope.names st Symbol.LocalType b.bind_name b.bind_span
        (decl_link_name text)
  end;
  st.out.local_decls <- Bind b :: st.out.local_decls

let rec find_up pick scope =
  match (pick scope, scope.parent) with
  | None, Some parent -> find_up pick parent
  | found, _ -> found

type lookup = Found of Symbol.t | Captured of Symbol.t | Missing

(* A lookup past a fn's own scopes only sees what a :: or a global declared *)
let find st name =
  let rec go outside scope =
    let outside = outside || Option.exists (( == ) scope) st.value_boundary in
    match (Names.find_opt scope.names name, scope.parent) with
    | Some sym, _ when outside && Symbol.is_local sym.Symbol.kind ->
        Captured sym
    | Some sym, _ -> Found sym
    | None, Some parent -> go outside parent
    | None, None -> Missing
  in
  go false st.scope

let use_symbol st span sym = Span.Table.replace st.out.syms span sym

(* The semantic pass reports what's wrong so a value is kept to say what it is *)
let use_struct_lit_name st name span =
  match find st name with
  | Found sym | Captured sym -> use_symbol st span sym
  | Missing -> ()

(* A type is a value too so semantic analysis says where it can't go *)
let use st what name span =
  match find st name with
  | Found sym -> use_symbol st span sym
  | Captured _ ->
      Diagnostic.emit (Diagnostic.error span "cannot capture variable");
      ignore (mint st Symbol.Error name span (Interner.text name) span)
  | Missing ->
      Diagnostic.emit (Diagnostic.error span "undefined %s" what);
      ignore (mint st Symbol.Error name span (Interner.text name) span)

(* Color.Red puts a type name where a value usually goes *)
let rec resolve_path st p =
  let { value = name; span } = Nonempty.hd p.owner in
  (* A nearer value wins so a local named str isn't the builtin type *)
  use st "variable" name span

(* A fn no :: names still needs a symbol to be called through *)
and resolve_expr st e =
  match e.edesc with
  | Ident name -> use st "variable" name e.espan
  | Call ({ edesc = Ident name; espan = span }, args) ->
      begin match find st name with
      | Found sym ->
          let func s =
            match Names.find_opt s.names name with
            | Some sym when Symbol.is_func sym.Symbol.kind -> Some sym
            | _ -> None
          in
          if not (Symbol.is_func sym.Symbol.kind) then
            Option.iter
              (Span.Table.replace st.out.shadowed span)
              (find_up func st.scope);
          use_symbol st span sym
      | Captured _ | Missing -> use st "function" name span
      end;
      List.iter (resolve_expr st) args
  | Call (callee, args) ->
      resolve_expr st callee;
      List.iter (resolve_expr st) args
  | BinOp (_, l, r)
  | Assign (_, l, r)
  | Range (l, r)
  | RangeInclusive (l, r)
  | Index (l, r) ->
      resolve_expr st l;
      resolve_expr st r
  | UnOp (_, e)
  | FieldAccess (e, _)
  | RangeFrom e
  | RangeTo e
  | RangeToInclusive e ->
      resolve_expr st e
  | Path segs -> resolve_path st segs
  | Cast (ty, inner) ->
      resolve_typ st ty;
      resolve_expr st inner
  | SizeOf ty -> resolve_typ st ty
  | ArrayLit elems -> List.iter (resolve_expr st) elems
  | StructLit (name, fields) ->
      Option.iter
        (fun { value; span } -> use_struct_lit_name st value span)
        name;
      List.iter (fun (_, e) -> resolve_expr st e) fields
  | Block body -> resolve_block st body
  | Match (scrutinee, arms) ->
      resolve_expr st scrutinee;
      List.iter (resolve_arm st) arms
  | If (branches, else_body) ->
      List.iter
        (fun (cond, { value = body; _ }) ->
          resolve_expr st cond;
          resolve_block st body)
        branches;
      Option.iter (fun { value = b; _ } -> resolve_block st b) else_body
  | Loop (_, _, body) -> resolve_block st body
  | For (_, { value = name; span = nspan }, iter, body) ->
      resolve_expr st iter;
      let st = enter_scope st in
      declare_local st Symbol.ForVar name nspan;
      resolve_block_contents st body
  | Binding ({ binder = RunTime; _ } as b) ->
      resolve_binding st b;
      declare_local st Symbol.Local b.bind_name.value b.bind_name.span
  | Binding ({ binder = CompileTime; _ } as b) -> resolve_binding st b
  | Return e | Break (_, e) -> Option.iter (resolve_expr st) e
  | Lambda l ->
      let name = Interner.intern "fn" in
      let link_name = Printf.sprintf "_Rlambda%d" !(st.next_id) in
      ignore (mint st Symbol.LocalFunc name e.espan link_name e.espan);
      resolve_lambda st l
  | PointerType t | SliceType t -> resolve_typ st t
  | ArrayType (n, t) ->
      resolve_expr st n;
      resolve_typ st t
  | FuncType (_, ps, ret) ->
      List.iter (resolve_typ st) ps;
      resolve_typ st ret
  | StructType fields -> List.iter (fun f -> resolve_typ st f.field_typ) fields
  | EnumType variants ->
      List.iter (fun v -> List.iter (resolve_typ st) v.variant_payload) variants
  | Int _ | Float _ | Bool _ | Null | Char _ | String _ | Unit | Hole | Extern _
  | RangeFull | DotVariant _ | Continue _ ->
      ()

(* A binding belongs to the arm it was written in so the scope opens first *)
and resolve_arm st a =
  let st = enter_scope st in
  resolve_pattern st a.pat;
  resolve_block_contents st a.arm_body.value

and resolve_pattern st p =
  match p.pdesc with
  | PatWild -> ()
  | PatValue e -> resolve_expr st e
  | PatBind name -> declare_local st Symbol.MatchBind name p.pspan
  | PatVariant (callee, subs) ->
      resolve_expr st callee;
      List.iter (resolve_pattern st) subs

(* A bare name is the one thing a type position reads differently *)
and resolve_typ st t =
  match t.edesc with
  | Ident name -> use st "type" name t.espan
  | _ -> resolve_expr st t

(* A :: of a fn is a function and any other :: is a type *)
and resolve_binding st b =
  let resolve_value e =
    match (b.binder, e.edesc) with
    | CompileTime, Lambda l -> resolve_lambda st l
    | CompileTime, _ -> resolve_typ st e
    | RunTime, _ -> resolve_expr st e
  in
  match b.bind_form with
  | Typed (t, e) ->
      resolve_typ st t;
      resolve_value e
  | Inferred e -> resolve_value e
  | Declared t -> resolve_typ st t

and resolve_block_contents st body =
  List.iter
    (function
      | { edesc = Binding ({ binder = CompileTime; _ } as b); _ } ->
          declare_local_bound st b
      | _ -> ())
    body;
  List.iter (resolve_expr st) body

and resolve_block st body = resolve_block_contents (enter_scope st) body

(* A fn can't see the enclosing body's variables *)
and resolve_lambda st l =
  let st = enter_scope { st with value_boundary = Some st.scope } in
  List.iter
    (fun p ->
      resolve_typ st p.param_typ;
      declare_local st Symbol.Param p.param_name.value p.param_span)
    l.lambda_params;
  Option.iter (resolve_typ st) l.lambda_ret;
  resolve_block_contents st l.lambda_body

(* A top level name lands first so a body can forward reference *)
let declare_decl st (Bind b) =
  let name = b.bind_name and span = b.bind_span in
  let text = Interner.text name.value in
  let mangled = decl_link_name text in
  match (b.binder, bind_value b) with
  | RunTime, _ -> declare_in st.top.names st Symbol.Global name span mangled
  | CompileTime, Some { edesc = Lambda { lambda_abi = NamedAbi _; _ }; _ } ->
      (* An export is there for C to call so it keeps its own name *)
      declare_in st.top.names st Symbol.Func name span text
  | CompileTime, Some { edesc = Extern _; _ } ->
      declare_in st.top.names st Symbol.Extern name span text
  | CompileTime, Some { edesc = Lambda _; _ } ->
      declare_in st.top.names st Symbol.Func name span mangled
  | CompileTime, (Some _ | None) ->
      declare_in st.top.names st Symbol.Type name span mangled

let resolve decls =
  let out = make_output () in
  let top = new_scope (Some out.prelude) in
  let st = { out; top; scope = top; value_boundary = None; next_id = ref 0 } in
  List.iter (declare_decl st) decls;
  List.iter (fun (Bind b) -> resolve_binding st b) decls;
  out
