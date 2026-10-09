(* SPDX-License-Identifier: Apache-2.0 *)

open Ast

(* Names are interned so a scope lookup is an int compare *)
module Names = Hashtbl.Make (struct
  type t = Ast.name

  let equal (a : t) (b : t) = a = b
  let hash t = t
end)

type namespace = Symbol.t Names.t

type scope = {
  values : namespace;
  items : namespace;
  types : namespace;
  parent : scope option;
}

type t = {
  syms : Symbol.t Span.Table.t;
  (* What a callee would have found if the value hadn't taken the name *)
  shadowed : Symbol.t Span.Table.t;
  prelude : scope;
  mutable local_decls : Ast.decl list;
}

type state = {
  out : t;
  top : scope;
  scope : scope;
  value_boundary : scope option;
  next_id : Symbol.id ref;
  header_hole : Ast.span option ref;
}

let prelude_symbol id name =
  {
    Symbol.id;
    name;
    link_name = name;
    kind = Symbol.Type;
    entry_point = false;
    span = Ast.dummy_span;
    name_span = Ast.dummy_span;
  }

let new_scope parent =
  {
    values = Names.create 16;
    items = Names.create 16;
    types = Names.create 16;
    parent;
  }

(* A builtin sits in the outermost scope so a program can shadow it like any name *)
let make_output () =
  let prelude = new_scope None in
  let seed id (name, _) =
    Names.replace prelude.types (Interner.intern name) (prelude_symbol id name);
    id - 1
  in
  (* We go negative so a builtin never grabs an id the program wants *)
  ignore
    (List.fold_left seed ((Symbol.unresolved_key :> int) - 1) Types.builtins);
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
  let entry (name, builtin) =
    let keyed sym = (Symbol.key sym, builtin) in
    Option.map keyed (Names.find_opt r.prelude.types (Interner.intern name))
  in
  List.filter_map entry Types.builtins

let main_name = Interner.intern "main"
let is_entry kind name = kind = Symbol.Func && name = main_name

let mint ?link_name ?name_span st kind name span =
  let id = !(st.next_id) in
  st.next_id := id + 1;
  let link_name = Option.value ~default:(Interner.text name) link_name in
  let sym =
    {
      Symbol.id;
      name = Interner.text name;
      link_name;
      kind;
      entry_point = is_entry kind name;
      span;
      name_span = Option.value ~default:span name_span;
    }
  in
  Span.Table.replace st.out.syms span sym;
  sym

(* The inner state goes out of scope with the block so none can stay open *)
let enter_scope st = { st with scope = new_scope (Some st.scope) }

(* The innermost scope holds params and top level body binders *)
let declare_local st kind name span =
  Names.replace st.scope.values name (mint st kind name span)

let declaration_link_name name =
  "_R" ^ string_of_int (String.length name) ^ name

let declare_in ?link_name table st kind (ident : Ast.ident) span =
  let name = ident.value in
  match Names.find_opt table name with
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

let declare_local_func st (ident : Ast.ident) span =
  let link_name =
    Printf.sprintf "_Rlocal%d_%s" !(st.next_id) (Interner.text ident.value)
  in
  declare_in ~link_name st.scope.items st Symbol.LocalFunc ident span

let value_or_item scope name =
  match Names.find_opt scope.values name with
  | Some sym -> Some sym
  | None -> Names.find_opt scope.items name

let rec find_type_in_scope scope name =
  match Names.find_opt scope.types name with
  | Some sym -> Some sym
  | None ->
      Option.bind scope.parent (fun parent -> find_type_in_scope parent name)

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
  | None -> Diagnostic.error span "undefined %s" what

let use_symbol st span sym = Span.Table.replace st.out.syms span sym
let find_type st name = find_type_in_scope st.scope name

let use_type st name span =
  match find_type st name with
  | Some sym -> use_symbol st span sym
  | None ->
      Diagnostic.emit (Diagnostic.error span "undefined type");
      ignore (mint st Symbol.Error name span)

(* Semantic analysis already reports an unknown struct literal *)
let use_type_if_found st name span =
  match find_type st name with Some sym -> use_symbol st span sym | None -> ()

let use st ~what name span =
  match lookup st name with
  | Some sym -> Span.Table.replace st.out.syms span sym
  | None when !(st.header_hole) = Some span ->
      ignore (mint st Symbol.Error name span)
  | None ->
      Diagnostic.emit (missing_value st ~what name span);
      ignore (mint st Symbol.Error name span)

let use_callee st ?(what = "function") name span =
  match (lookup st name, find_type st name) with
  | None, Some sym -> use_symbol st span sym
  | value, _ ->
      (match value with
      | Some sym when not (Symbol.is_func sym.Symbol.kind) ->
          Option.iter
            (Span.Table.replace st.out.shadowed span)
            (find_func_in_scope st.scope name)
      | Some _ | None -> ());
      use st ~what name span

(* Body binders can redeclare but params can't repeat *)
let declare_param st p =
  let name = p.param_name.value in
  match Names.find_opt st.scope.values name with
  | Some prev ->
      Diagnostic.emit
        (Diagnostic.error p.param_span "already defined"
        |> Diagnostic.secondary prev.Symbol.span "previous definition here");
      Span.Table.replace st.out.syms p.param_span prev
  | None -> declare_local st Symbol.Param name p.param_span

(* Color.Red puts a type name where a value usually goes *)
let use_type_name st name span =
  (* A nearer value wins so a local named str isn't the builtin type *)
  if lookup st name <> None then false
  else
    match find_type st name with
    | None -> false
    | Some sym ->
        use_symbol st span sym;
        true

let resolve_missing_value_root st name span =
  match find_type_in_scope st.scope name with
  | Some _ -> false
  | None ->
      use st ~what:"variable" name span;
      true

let resolve_value_root st p =
  let { Ast.value = name; span } = Nonempty.hd p.Ast.owner in
  match lookup st name with
  | Some sym ->
      use_symbol st span sym;
      true
  | None -> resolve_missing_value_root st name span

let rec resolve_path st p =
  if not (resolve_value_root st p) then
    let prefix = Ast.owner_expr p in
    match prefix.Ast.desc with
    | Ident name when use_type_name st name prefix.Ast.span -> ()
    | _ -> resolve_expr st prefix

(* The name a header ends on is the one a struct literal would have opened *)
and rightmost e =
  match e.Ast.desc with
  | BinOp (_, _, r) | Assign (_, _, r) | Range (_, r) | RangeInclusive (_, r) ->
      rightmost r
  | UnOp (_, r) | RangeFrom r -> rightmost r
  | _ -> e

(* The parser gave the brace to the body so the name is left standing alone *)
and resolve_header st e =
  let tail = rightmost e in
  (match tail.Ast.desc with
  | Ident name
    when lookup st name = None && find_type_in_scope st.scope name <> None ->
      st.header_hole := Some tail.Ast.span;
      Diagnostic.emit
        (Diagnostic.error tail.Ast.span "expected a value and found a type"
        |> Diagnostic.help "wrap a struct literal in parentheses here")
  | _ -> ());
  resolve_expr st e;
  st.header_hole := None

and resolve_expr st e =
  match e.desc with
  | Ident name -> use st ~what:"variable" name e.span
  | Call ({ desc = Ident name; span }, args) ->
      use_callee st name span;
      List.iter (resolve_expr st) args
  | Call ({ desc = Path segs; _ }, args) ->
      resolve_path st segs;
      List.iter (resolve_expr st) args
  | Call (callee, args) ->
      resolve_expr st callee;
      List.iter (resolve_expr st) args
  | BinOp (_, l, r) | Assign (_, l, r) ->
      resolve_expr st l;
      resolve_expr st r
  | UnOp (_, inner) -> resolve_expr st inner
  | Range (l, r) | RangeInclusive (l, r) ->
      resolve_expr st l;
      resolve_expr st r
  | RangeFrom e | RangeTo e | RangeToInclusive e -> resolve_expr st e
  | RangeFull -> ()
  | Path segs -> resolve_path st segs
  | FieldAccess (inner, _) -> resolve_expr st inner
  | Cast (ty, inner) ->
      resolve_typ st ty;
      resolve_expr st inner
  | SizeOf ty -> resolve_typ st ty
  | Index (base, idx) ->
      resolve_expr st base;
      resolve_expr st idx
  | ArrayLit elems -> List.iter (resolve_expr st) elems
  | StructLit ({ value = name; span = name_span }, fields) ->
      use_type_if_found st name name_span;
      List.iter (fun (_, e) -> resolve_expr st e) fields
  | Block body -> resolve_block st body
  | Match (scrutinee, arms) ->
      resolve_header st scrutinee;
      List.iter (resolve_arm st) arms
  | If (branches, else_body) ->
      List.iter
        (fun (cond, { Ast.value = body; _ }) ->
          resolve_header st cond;
          resolve_block st body)
        branches;
      Option.iter (fun { Ast.value = b; _ } -> resolve_block st b) else_body
  | While (_, cond, body) ->
      resolve_header st cond;
      resolve_block st body
  | Loop (_, body) -> resolve_block st body
  | For (_, { value = name; span = nspan }, iter, body) ->
      resolve_header st iter;
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

and resolve_typ st t =
  match t.tdesc with
  | Named name -> use_type st name t.tspan
  | Pointer t | Slice t -> resolve_typ st t
  | Array (e, t) ->
      resolve_expr st e;
      resolve_typ st t
  | FuncPtr (_, ps, ret) ->
      List.iter (resolve_typ st) ps;
      Option.iter (resolve_typ st) ret
  | UnitType -> ()

and declare_block_item st d =
  (match d with
  | LocalStruct sd -> declare_local_type st sd.struct_name sd.struct_span
  | LocalTypeAlias td -> declare_local_type st td.alias_name td.alias_span
  | LocalFunc fd -> declare_local_func st fd.func_name fd.func_span
  | LocalEnum ed -> declare_local_type st ed.enum_name ed.enum_span);
  st.out.local_decls <- decl_of_local d :: st.out.local_decls

and resolve_local_decl st d =
  match d with
  | LocalFunc fd -> resolve_local_func st fd
  | LocalStruct _ | LocalTypeAlias _ | LocalEnum _ ->
      resolve_decl st (decl_of_local d)

and resolve_block_item st = function
  | Expr e -> resolve_expr st e
  | Decl d -> resolve_local_decl st d

and resolve_block_contents st body =
  List.iter (function Expr _ -> () | Decl d -> declare_block_item st d) body;
  List.iter (resolve_block_item st) body

and resolve_block st body = resolve_block_contents (enter_scope st) body

and resolve_func st fd =
  let st = enter_scope st in
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
let foreign_link_name (fd : Ast.func_def) =
  if fd.extern_abi <> Ast.NoAbi then Some (Interner.text fd.func_name.value)
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
  let st =
    {
      out;
      top;
      scope = top;
      value_boundary = None;
      next_id = ref 0;
      header_hole = ref None;
    }
  in
  declare_decls st decls;
  List.iter (resolve_decl st) decls;
  out
