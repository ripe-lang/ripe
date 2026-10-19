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
(* A param or arm name can't repeat and a :: keeps its name for the whole block *)
let declare_local st kind name span =
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
        )
  | None ->
      let link_name =
        Option.value
          ~default:(declaration_link_name (Interner.text name))
          link_name
      in
      Names.replace table name
        (mint ~link_name ~name_span:ident.span st kind name span)

let declare_global ?link_name st kind ident span =
  declare_in ?link_name st.top.values st kind ident span

let declare_type st ident span =
  declare_in st.top.types st Symbol.Type ident span

let declare_local_type st ident span =
  declare_in st.scope.types st Symbol.LocalType ident span

let declare_local_func st (ident : ident) span =
  let link_name =
    Printf.sprintf "_Rlocal%d_%s" !(st.next_id) (Interner.text ident.value)
  in
  declare_in ~link_name st.scope.items st Symbol.LocalFunc ident span

let value_or_item scope name =
  match Names.find_opt scope.values name with
  | Some sym -> Some sym
  | None -> Names.find_opt scope.items name

let rec find_type scope name =
  match Names.find_opt scope.types name with
  | Some sym -> Some sym
  | None -> Option.bind scope.parent (fun parent -> find_type parent name)

let rec find_value scope name =
  match value_or_item scope name with
  | Some sym -> Some sym
  | None -> Option.bind scope.parent (fun parent -> find_value parent name)

(* Top level funcs sit in values and the local ones sit in items *)
let rec find_func_in_scope scope name =
  let here =
    match Names.find_opt scope.values name with
    | Some sym when Symbol.is_func sym.Symbol.kind -> Some sym
    | _ -> Names.find_opt scope.items name
  in
  match here with
  | Some _ -> here
  | None ->
      Option.bind scope.parent (fun parent -> find_func_in_scope parent name)

let is_item_value sym =
  match sym.Symbol.kind with
  | Symbol.Func | Symbol.Extern | Symbol.Global | Symbol.LocalFunc -> true
  | Symbol.Error | Symbol.Type | Symbol.LocalType | Symbol.Local | Symbol.Param
  | Symbol.ForVar | Symbol.MatchBind ->
      false

let rec find_item_value scope name =
  match Names.find_opt scope.items name with
  | Some sym -> Some sym
  | None -> find_item_value_or_parent scope name

and find_item_value_or_parent scope name =
  match Names.find_opt scope.values name with
  | Some sym when is_item_value sym -> Some sym
  | Some _ -> None
  | None -> Option.bind scope.parent (fun parent -> find_item_value parent name)

let boundary_value boundary scope name =
  if scope == boundary then find_item_value boundary name
  else value_or_item scope name

let rec find_before_boundary boundary scope name =
  match boundary_value boundary scope name with
  | Some sym -> Some sym
  | None when scope == boundary -> None
  | None ->
      Option.bind scope.parent (fun parent ->
          find_before_boundary boundary parent name)

let lookup st name =
  match st.value_boundary with
  | Some boundary -> find_before_boundary boundary st.scope name
  | None -> find_value st.scope name

let captured_in name boundary =
  match find_value boundary name with
  | Some sym when not (is_item_value sym) -> Some sym
  | Some _ | None -> None

let captured_value st name = Option.bind st.value_boundary (captured_in name)

let missing_value st ~what name span =
  match captured_value st name with
  | Some _ -> Diagnostic.error span "local function cannot capture variable"
  | None -> (
      match find_type st.scope name with
      | Some sym ->
          Diagnostic.error span "expected a value"
          |> Diagnostic.label "this names %s"
               (Symbol.describe_kind sym.Symbol.kind)
      | None -> Diagnostic.error span "undefined %s" what)

let use_symbol st span sym = Span.Table.replace st.out.syms span sym

(* The semantic pass reports what's wrong so a value is kept to say what it is *)
let use_struct_lit_name st name span =
  match find st name with
  | Found sym -> use_symbol st span sym
  | Captured | Missing -> ()

(* A type is a value too so semantic analysis says where it can't go *)
let use st what name span =
  match find st name with
  | Found sym -> use_symbol st span sym
  | (Captured | Missing) as missed ->
      Diagnostic.emit
        (if missed = Captured then
           Diagnostic.error span "cannot capture variable"
         else Diagnostic.error span "undefined %s" what);
      ignore (mint st Symbol.Error name span (Interner.text name) span)

(* Color.Red puts a type name where a value usually goes *)
(* A nearer value wins so a local named str isn't the builtin type *)
let rec resolve_path st p =
  let { value = name; span } = Nonempty.hd p.owner in
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
      | Captured | Missing -> use st "function" name span
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
  | Binding ({ value = name; span = nspan }, ann, e) ->
      Option.iter (resolve_typ st) ann;
      Option.iter (resolve_expr st) e;
      declare_local st Symbol.Local name nspan
  | Return e -> Option.iter (resolve_expr st) e
  | Break (_, value) -> Option.iter (resolve_expr st) value
  | Continue _ -> ()
  | Int _ | Float _ | Bool _ | Null | Char _ | String _ -> ()
  | Unit -> ()

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
      declare_param st p)
    fd.params;
  Option.iter (resolve_typ st) fd.ret;
  resolve_block_contents st fd.body

(* A local fn can't see the enclosing body's variables *)
and resolve_local_func st fd =
  resolve_func { st with value_boundary = Some st.scope } fd

and resolve_decl st = function
  | Func fd | Extern fd -> resolve_func st fd
  | Global gd ->
      Option.iter (resolve_typ st) gd.typ;
      Option.iter (resolve_expr st) gd.init
  | Struct sd -> List.iter (fun f -> resolve_typ st f.field_typ) sd.fields
  | TypeAlias td -> resolve_typ st td.alias_typ
  (* TODO(c111): nothing to walk until a variant can hold a type *)
  | Enum _ -> ()

(* An extern fn with a body is there for C to call so it keeps its own name *)
let foreign_link_name (fd : func_def) =
  if fd.extern_abi <> NoAbi then Some (Interner.text fd.func_name.value)
  else None

(* A top level name lands first so a body can forward reference *)
let declare_decls st decls =
  List.iter
    (function
      | Func fd ->
          declare_global ?link_name:(foreign_link_name fd) st Symbol.Func
            fd.func_name fd.func_span
      | Extern fd ->
          declare_global
            ~link_name:(Interner.text fd.func_name.value)
            st Symbol.Extern fd.func_name fd.func_span
      | Global gd -> declare_global st Symbol.Global gd.name gd.span
      | Struct sd -> declare_type st sd.struct_name sd.struct_span
      | TypeAlias td -> declare_type st td.alias_name td.alias_span
      | Enum ed -> declare_type st ed.enum_name ed.enum_span)
    decls

let resolve decls =
  let out = make_output () in
  let top = new_scope (Some out.prelude) in
  let st = { out; top; scope = top; value_boundary = None; next_id = ref 0 } in
  List.iter (declare_decl st) decls;
  List.iter (fun (Bind b) -> resolve_binding st b) decls;
  out
