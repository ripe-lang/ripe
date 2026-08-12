(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast

(* Names are interned so a scope lookup is an int compare *)
module Names = Hashtbl.Make (struct
  type t = Ast.name

  let equal (a : t) (b : t) : bool = a = b
  let hash (t : t) : int = t
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
  module_paths : (Symbol.module_id, string list) Hashtbl.t;
  (* Sorted by base so a position finds its module by search *)
  mutable file_modules : (int * Symbol.module_id) array;
  imports : (Symbol.module_id * Ast.name list, scope) Hashtbl.t;
  failed_imports : (Symbol.module_id * Ast.name list, unit) Hashtbl.t;
  prelude : scope;
  mutable local_decls : Ast.decl list;
}

type state = {
  out : t;
  module_id : Symbol.module_id;
  module_path : string list;
  qualify : bool;
  is_root : bool;
  top : scope;
  mutable scope : scope;
  mutable value_boundary : scope option;
  mutable next_id : Symbol.id;
  diags : Diagnostic.sink;
}

type resolved_program = { uses : t; decls : Ast.decl list }

type qualified =
  | Local
  | Found of Symbol.t
  | Missing of Ast.name list * Ast.name

let prelude_symbol (id : Symbol.id) (name : string) : Symbol.t =
  {
    Symbol.id;
    module_id = Symbol.prelude_module_id;
    name;
    link_name = name;
    kind = Symbol.Type;
    visibility = Symbol.Public;
    entry_point = false;
    span = Ast.dummy_span;
    name_span = Ast.dummy_span;
  }

let new_scope (parent : scope option) : scope =
  {
    values = Names.create 16;
    items = Names.create 16;
    types = Names.create 16;
    parent;
  }

(* Real code averages one symbol per 7 to 11 bytes so sizing up front skips
   about eight rehashes *)
let bytes_per_symbol = 8

(* A builtin sits in the outermost scope so a module can shadow it like any name *)
let make_output ~(source_bytes : int) (modules : int) : t =
  let prelude = new_scope None in
  let seed id (name, _) =
    Names.replace prelude.types (Interner.intern name) (prelude_symbol id name)
  in
  List.iteri seed Types.builtins;
  let out =
    {
      syms = Span.Table.create (source_bytes / bytes_per_symbol);
      module_paths = Hashtbl.create modules;
      file_modules = [||];
      imports = Hashtbl.create modules;
      failed_imports = Hashtbl.create modules;
      prelude;
      local_decls = [];
    }
  in
  Hashtbl.add out.module_paths Symbol.prelude_module_id [];
  out

(* This is the `--emit resolve` output *)
let dump (r : t) : string =
  Span.Table.to_seq r.syms |> List.of_seq
  |> List.sort (fun ((a : Ast.span), _) ((b : Ast.span), _) -> compare a b)
  |> List.map (fun ((sp : Ast.span), (s : Symbol.t)) ->
      Printf.sprintf "(%d,%d) -> #%d.%d %s %s\n" (Span.lo sp) (Span.hi sp)
        s.Symbol.module_id s.Symbol.id
        (Symbol.show_kind s.Symbol.kind)
        s.Symbol.name)
  |> String.concat ""

let sym_at (r : t) (span : Ast.span) : Symbol.t =
  match Span.Table.find_opt r.syms span with
  | Some s -> s
  | None -> Diagnostic.ice ~span "no symbol resolved here"

let sym_at_opt (r : t) (span : Ast.span) : Symbol.t option =
  Span.Table.find_opt r.syms span

let qname_of (r : t) (s : Symbol.t) : Qname.t =
  let path = Hashtbl.find r.module_paths s.Symbol.module_id in
  Qname.make (Symbol.key s) path s.Symbol.name

let local_decls (r : t) : Ast.decl list = List.rev r.local_decls

let builtins (r : t) : (Symbol.key * Types.builtin) list =
  let entry (name, builtin) =
    let keyed (sym : Symbol.t) = (Symbol.key sym, builtin) in
    Option.map keyed (Names.find_opt r.prelude.types (Interner.intern name))
  in
  List.filter_map entry Types.builtins

(* A message inside math says add where one outside says math.add *)
let module_at (r : t) (pos : int) : Symbol.module_id option =
  let bases = r.file_modules in
  let rec search lo hi =
    if lo > hi then None
    else
      let mid = (lo + hi) / 2 in
      let base, module_id = bases.(mid) in
      if base > pos then search lo (mid - 1)
      else if mid + 1 < Array.length bases && fst bases.(mid + 1) <= pos then
        search (mid + 1) hi
      else Some module_id
  in
  if Array.length bases = 0 || pos < 0 then None
  else search 0 (Array.length bases - 1)

let module_path_at (r : t) (span : Ast.span) : string list =
  match module_at r (Span.lo span) with
  | None -> []
  | Some module_id -> (
      match Hashtbl.find_opt r.module_paths module_id with
      | Some path -> path
      | None -> [])

let main_name : Ast.name = Interner.intern "main"

let is_entry (st : state) (kind : Symbol.kind) (name : Ast.name) : bool =
  kind = Symbol.Func && st.is_root && name = main_name

(* An extern and the entry point are named by something outside the compiler *)
let declaration_link_name (st : state) (kind : Symbol.kind) (name : Ast.name) :
    string =
  let text = Interner.text name in
  if not st.qualify then text
  else if is_entry st kind name then text
  else
    match kind with
    | Symbol.Extern -> text
    | _ -> Mangle.declaration st.module_path text

let mint ?(visibility = Symbol.Private) ?link_name ?name_span (st : state)
    (kind : Symbol.kind) (name : Ast.name) (span : Ast.span) : Symbol.t =
  let id = st.next_id in
  st.next_id <- id + 1;
  let link_name = Option.value ~default:(Interner.text name) link_name in
  let sym =
    {
      Symbol.id;
      module_id = st.module_id;
      name = Interner.text name;
      link_name;
      kind;
      visibility;
      entry_point = is_entry st kind name;
      span;
      name_span = Option.value ~default:span name_span;
    }
  in
  Span.Table.replace st.out.syms span sym;
  sym

let push_scope (st : state) = st.scope <- new_scope (Some st.scope)

let pop_scope (st : state) =
  st.scope <-
    (match st.scope.parent with Some parent -> parent | None -> assert false)

(* The innermost scope holds params and top level body binders *)
let declare_local (st : state) kind name span : unit =
  Names.replace st.scope.values name (mint st kind name span)

let declare_in ?link_name ?name_span (table : namespace) (st : state)
    (kind : Symbol.kind) (visibility : Symbol.visibility) (name : Ast.name)
    (span : Ast.span) : unit =
  match Names.find_opt table name with
  | Some prev ->
      Diagnostic.emit st.diags
        (Diagnostic.redefinition
           (Option.value ~default:span name_span)
           ~prev:prev.Symbol.name_span)
  | None ->
      let link_name =
        Option.value ~default:(declaration_link_name st kind name) link_name
      in
      Names.replace table name
        (mint ~visibility ~link_name ?name_span st kind name span)

let declare_global ?name_span (st : state) kind visibility name span : unit =
  declare_in ?name_span st.top.values st kind visibility name span

let declare_type ?name_span (st : state) visibility name span : unit =
  declare_in ?name_span st.top.types st Symbol.Type visibility name span

let declare_local_type (st : state) name span : unit =
  declare_in st.scope.types st Symbol.LocalType Symbol.Private name span

let declare_local_func (st : state) (name : Ast.name) span : unit =
  let link_name =
    Printf.sprintf "_Rlocal%d_%d_%s" st.module_id st.next_id
      (Interner.text name)
  in
  declare_in ~link_name st.scope.items st Symbol.LocalFunc Symbol.Private name
    span

let rec find_type_in_chain (scope : scope) (name : Ast.name) : Symbol.t option =
  match Names.find_opt scope.types name with
  | Some sym -> Some sym
  | None ->
      Option.bind scope.parent (fun parent -> find_type_in_chain parent name)

let value_or_item (scope : scope) (name : Ast.name) : Symbol.t option =
  match Names.find_opt scope.values name with
  | Some sym -> Some sym
  | None -> Names.find_opt scope.items name

let rec find_value (scope : scope) (name : Ast.name) : Symbol.t option =
  match value_or_item scope name with
  | Some sym -> Some sym
  | None -> Option.bind scope.parent (fun parent -> find_value parent name)

let is_item_value (sym : Symbol.t) : bool =
  match sym.Symbol.kind with
  | Symbol.Func | Symbol.Extern | Symbol.Global _ | Symbol.LocalFunc
  | Symbol.Module ->
      true
  | Symbol.Error | Symbol.Type | Symbol.LocalType | Symbol.Local _
  | Symbol.Param | Symbol.ForVar ->
      false

let rec find_item_value (scope : scope) (name : Ast.name) : Symbol.t option =
  match Names.find_opt scope.items name with
  | Some sym -> Some sym
  | None -> (
      match Names.find_opt scope.values name with
      | Some sym when is_item_value sym -> Some sym
      | Some _ -> None
      | None ->
          Option.bind scope.parent (fun parent -> find_item_value parent name))

let rec find_before_boundary (boundary : scope) (scope : scope)
    (name : Ast.name) : Symbol.t option =
  if scope == boundary then find_item_value boundary name
  else
    match value_or_item scope name with
    | Some sym -> Some sym
    | None ->
        Option.bind scope.parent (fun parent ->
            find_before_boundary boundary parent name)

let lookup (st : state) (name : Ast.name) : Symbol.t option =
  match st.value_boundary with
  | Some boundary -> find_before_boundary boundary st.scope name
  | None -> find_value st.scope name

let captured_value (st : state) (name : Ast.name) : Symbol.t option =
  Option.bind st.value_boundary (fun boundary ->
      match find_value boundary name with
      | Some sym when not (is_item_value sym) -> Some sym
      | Some _ | None -> None)

let missing_value (st : state) ~(what : string) name span : Diagnostic.t =
  match captured_value st name with
  | Some _ -> Diagnostic.error_at span "local function cannot capture variable"
  | None -> Diagnostic.undefined_name span what

(* The name comes off the symbol not off the spelling at the use site *)
let check_visibility (st : state) (span : Ast.span) (sym : Symbol.t) : unit =
  if
    sym.Symbol.module_id <> st.module_id
    && sym.Symbol.visibility = Symbol.Private
  then
    Diagnostic.emit st.diags
      (Diagnostic.error_at span "private declaration"
      |> Diagnostic.secondary sym.Symbol.span "declared private here")

let use_symbol (st : state) (span : Ast.span) (sym : Symbol.t) : unit =
  check_visibility st span sym;
  Span.Table.replace st.out.syms span sym

let split_member (path : Ast.name list) : (Ast.name list * Ast.name) option =
  match List.rev path with
  | member :: rest -> Some (List.rev rest, member)
  | [] -> None

(* A nearer value binding wins so a local named math is not a module *)
let find_module (st : state) (path : Ast.name list) : scope option =
  match path with
  | [] -> None
  | root :: _ -> (
      match lookup st root with
      | Some { Symbol.kind = Symbol.Module; _ } ->
          Hashtbl.find_opt st.out.imports (st.module_id, path)
      | Some _ | None -> None)

let failed_import (st : state) (path : Ast.name list) : bool =
  Hashtbl.mem st.out.failed_imports (st.module_id, path)

(* math.Vec goes through the import and Vec walks out to the builtins *)
let find_type (st : state) (path : Ast.name list) (name : Ast.name) :
    Symbol.t option =
  if path = [] then find_type_in_chain st.scope name
  else
    let member (scope : scope) = Names.find_opt scope.types name in
    Option.bind (find_module st path) member

let use_type (st : state) (path : Ast.name list) (name : Ast.name)
    (span : Ast.span) : unit =
  match find_type st path name with
  | Some sym -> use_symbol st span sym
  | None ->
      let shown = Ast.show_named path name in
      if not (failed_import st path) then
        Diagnostic.emit st.diags (Diagnostic.undefined_name span "type");
      ignore (mint st Symbol.Error (Interner.intern shown) span)

(* The typechecker already reports an unknown struct literal *)
let use_type_if_found (st : state) (name : Ast.name) (span : Ast.span) : unit =
  match find_type st [] name with
  | Some sym -> use_symbol st span sym
  | None -> ()

let use (st : state) ~(what : string) name span : unit =
  match lookup st name with
  | Some sym -> Span.Table.replace st.out.syms span sym
  | None ->
      Diagnostic.emit st.diags (missing_value st ~what name span);
      ignore (mint st Symbol.Error name span)

(* Body binders can redeclare but params can't repeat *)
let declare_param (st : state) (p : param) : unit =
  match Names.find_opt st.scope.values p.param_name with
  | Some prev ->
      Diagnostic.emit st.diags
        (Diagnostic.redefinition p.param_span ~prev:prev.Symbol.span);
      Span.Table.replace st.out.syms p.param_span prev
  | None -> declare_local st Symbol.Param p.param_name p.param_span

let rec access_path (e : expr) : Ast.name list option =
  match e.desc with
  | Ident name -> Some [ name ]
  | FieldAccess (inner, name, _) ->
      Option.map (fun path -> path @ [ name ]) (access_path inner)
  | _ -> None

let qualified_use (st : state) (e : expr) : qualified =
  match Option.bind (access_path e) split_member with
  | None -> Local
  | Some (module_path, member) -> (
      match find_module st module_path with
      | None -> Local
      | Some scope -> (
          match Names.find_opt scope.values member with
          | Some sym -> Found sym
          | None -> Missing (module_path, member)))

let use_qualified (st : state) ~(what : string) (e : expr) : bool =
  match qualified_use st e with
  | Local -> false
  | Found sym ->
      use_symbol st e.span sym;
      true
  | Missing (module_path, member) ->
      (* The import already failed so every name under it would say the same thing twice *)
      if not (failed_import st module_path) then
        Diagnostic.emit st.diags (Diagnostic.undefined_name e.span what);
      (* The stages after this read a symbol back off every span they walk *)
      ignore
        (mint st Symbol.Error
           (Interner.intern (Ast.show_named module_path member))
           e.span);
      true

(* Color.Red puts a type name where a value usually goes *)
let use_type_name (st : state) (e : expr) : bool =
  match Option.bind (access_path e) split_member with
  | None -> false
  | Some (path, name) -> (
      if
        (* A nearer value wins so a local named str isn't the builtin type *)
        path = [] && lookup st name <> None
      then false
      else
        match find_type st path name with
        | None -> false
        | Some sym ->
            use_symbol st e.span sym;
            true)

let rec resolve_expr (st : state) (e : expr) : unit =
  match e.desc with
  | ErrorExpr -> ()
  | Ident name -> use st ~what:"variable" name e.span
  | Call (({ desc = Ident name; span } as callee), args) ->
      if not (use_qualified st ~what:"function" callee) then
        use st ~what:"function" name span;
      List.iter (resolve_expr st) args
  | Call (callee, args) ->
      if not (use_qualified st ~what:"function" callee) then
        resolve_expr st callee;
      List.iter (resolve_expr st) args
  | BinOp (_, l, r) ->
      resolve_expr st l;
      resolve_expr st r
  | UnOp (_, inner) -> resolve_expr st inner
  | Range (l, r) | RangeInclusive (l, r) ->
      resolve_expr st l;
      resolve_expr st r
  | RangeFrom e | RangeTo e | RangeToInclusive e -> resolve_expr st e
  | RangeFull -> ()
  | FieldAccess (inner, _, _) ->
      if not (use_qualified st ~what:"variable" e) then
        if not (use_type_name st inner) then resolve_expr st inner
  | Cast (inner, ty) ->
      resolve_expr st inner;
      resolve_typ st ty
  | SizeOf ty -> resolve_typ st ty
  | Index (base, idx) ->
      resolve_expr st base;
      resolve_expr st idx
  | ArrayLit elems -> List.iter (resolve_expr st) elems
  | StructLit (path, name, name_span, fields) ->
      if path = [] then use_type_if_found st name name_span
      else use_type st path name name_span;
      List.iter (fun (_, _, e) -> resolve_expr st e) fields
  | Block body -> resolve_block st body
  | Match (scrutinee, arms) ->
      resolve_expr st scrutinee;
      List.iter (resolve_arm st) arms
  | If (branches, else_body) ->
      List.iter
        (fun (cond, { Ast.value = body; _ }) ->
          resolve_expr st cond;
          resolve_block st body)
        branches;
      Option.iter (fun { Ast.value = b; _ } -> resolve_block st b) else_body
  | While (_, cond, body) ->
      resolve_expr st cond;
      resolve_block st body
  | Loop (_, body) -> resolve_block st body
  | For (_, name, nspan, iter, body) ->
      resolve_expr st iter;
      push_scope st;
      declare_local st Symbol.ForVar name nspan;
      resolve_block_contents st body;
      pop_scope st
  | Binding (kind, name, nspan, ann, e) ->
      Option.iter (resolve_typ st) ann;
      Option.iter (resolve_expr st) e;
      declare_local st (Symbol.Local kind) name nspan
  | Return e -> Option.iter (resolve_expr st) e
  | Break (_, value) -> Option.iter (resolve_expr st) value
  | Continue _ -> ()
  | Int _ | Float _ | Bool _ | Null | Char _ | String _ | Undefined -> ()
  | PairAssign (ft, st', fv, sv) ->
      resolve_expr st ft;
      resolve_expr st st';
      resolve_expr st fv;
      resolve_expr st sv

(* A binding belongs to the arm it was written in so the scope opens first *)
and resolve_arm (st : state) (a : arm) : unit =
  push_scope st;
  resolve_pattern st a.pat;
  resolve_block_contents st a.arm_body.value;
  pop_scope st

and resolve_pattern (st : state) (p : pattern) : unit =
  match p.pdesc with
  | PatWild -> ()
  | PatValue e -> resolve_expr st e
  (* A name already standing for a constant compares against it so only a name that means nothing yet becomes a binding *)
  | PatBind name -> (
      match lookup st name with
      | Some sym when Symbol.is_comptime sym.Symbol.kind ->
          use_symbol st p.pspan sym
      | Some _ | None -> declare_local st (Symbol.Local Ast.Let) name p.pspan)

(* An array size expression may name constants *)
and resolve_typ (st : state) (t : typ) : unit =
  match t.tdesc with
  | ErrorType -> ()
  | Named (path, name) -> use_type st path name t.tspan
  | Pointer t | Slice t -> resolve_typ st t
  | Array (e, t) ->
      resolve_expr st e;
      resolve_typ st t
  | FuncPtr (_, ps, ret) ->
      List.iter (resolve_typ st) ps;
      Option.iter (resolve_typ st) ret

and declare_block_item (st : state) (d : local_decl) : unit =
  (match d with
  | LocalStruct sd -> declare_local_type st sd.struct_name sd.struct_span
  | LocalTypeAlias td | LocalNewtype td ->
      declare_local_type st td.alias_name td.alias_span
  | LocalFunc fd -> declare_local_func st fd.func_name fd.func_span
  | LocalEnum ed -> declare_local_type st ed.enum_name ed.enum_span);
  st.out.local_decls <- decl_of_local d :: st.out.local_decls

and resolve_local_decl (st : state) (d : local_decl) : unit =
  match d with
  | LocalFunc fd -> resolve_local_func st fd
  | LocalStruct _ | LocalTypeAlias _ | LocalNewtype _ | LocalEnum _ ->
      resolve_decl st (decl_of_local d)

and resolve_block_item (st : state) = function
  | Expr e -> resolve_expr st e
  | Decl d -> resolve_local_decl st d

and resolve_block_contents (st : state) (body : block) : unit =
  List.iter (function Expr _ -> () | Decl d -> declare_block_item st d) body;
  List.iter (resolve_block_item st) body

and resolve_block (st : state) (body : block) : unit =
  push_scope st;
  resolve_block_contents st body;
  pop_scope st

and resolve_func (st : state) (fd : func_def) : unit =
  push_scope st;
  List.iter
    (fun (p : param) ->
      resolve_typ st p.param_typ;
      declare_param st p)
    fd.params;
  Option.iter (resolve_typ st) fd.ret;
  resolve_block_contents st fd.body;
  pop_scope st

and resolve_local_func (st : state) (fd : func_def) : unit =
  let outer = st.scope in
  let saved_boundary = st.value_boundary in
  st.value_boundary <- Some outer;
  resolve_func st fd;
  st.value_boundary <- saved_boundary

and resolve_decl (st : state) : decl -> unit = function
  | Func fd | Extern fd -> resolve_func st fd
  | Global gd ->
      resolve_typ st gd.typ;
      Option.iter (resolve_expr st) gd.init
  | Struct sd ->
      List.iter (fun (f : field) -> resolve_typ st f.field_typ) sd.fields
  | TypeAlias td | Newtype td -> resolve_typ st td.alias_typ
  (* TODO: nothing to walk until a variant can hold a type *)
  | Enum _ -> ()

let visibility modifiers =
  if List.mem Ast.Pub modifiers then Symbol.Public else Symbol.Private

let make_state ~(out : t) ~(diags : Diagnostic.sink)
    ~(module_id : Symbol.module_id) ~(module_path : string list)
    ~(qualify : bool) ~(is_root : bool) : state =
  let top = new_scope (Some out.prelude) in
  {
    out;
    module_id;
    module_path;
    qualify;
    is_root;
    top;
    scope = top;
    value_boundary = None;
    next_id = 0;
    diags;
  }

(* A top level name lands first so a body can forward reference *)
let declare_decls (st : state) (decls : decl list) : unit =
  List.iter
    (function
      | Func fd ->
          declare_global ~name_span:fd.func_name_span st Symbol.Func
            (visibility fd.func_modifiers)
            fd.func_name fd.func_span
      | Extern fd ->
          declare_global ~name_span:fd.func_name_span st Symbol.Extern
            Symbol.Private fd.func_name fd.func_span
      | Global gd ->
          declare_global ~name_span:gd.name_span st (Symbol.Global gd.kind)
            (visibility gd.modifiers) gd.name gd.span
      | Struct sd ->
          declare_type ~name_span:sd.struct_name_span st
            (visibility sd.struct_modifiers)
            sd.struct_name sd.struct_span
      | TypeAlias td | Newtype td ->
          declare_type ~name_span:td.alias_name_span st
            (visibility td.alias_modifiers)
            td.alias_name td.alias_span
      | Enum ed ->
          declare_type ~name_span:ed.enum_name_span st
            (visibility ed.enum_modifiers)
            ed.enum_name ed.enum_span)
    decls

let resolve ~(diags : Diagnostic.sink) ~(module_id : Symbol.module_id)
    (decls : decl list) : t =
  let out = make_output ~source_bytes:0 1 in
  Hashtbl.add out.module_paths module_id [];
  let st =
    make_state ~out ~diags ~module_id ~module_path:[] ~qualify:false
      ~is_root:true
  in
  declare_decls st decls;
  List.iter (resolve_decl st) decls;
  out

let resolve_program ~(diags : Diagnostic.sink) (program : Program.t) :
    resolved_program =
  let count = Array.length program.Program.modules in
  let source_bytes =
    Array.fold_left
      (fun total (m : Program.module_) ->
        List.fold_left
          (fun total (u : Program.unit_) ->
            total
            + String.length (Source_map.src u.Program.source.Program.source_map))
          total m.Program.units)
      0 program.Program.modules
  in
  let out = make_output ~source_bytes count in
  let states = Hashtbl.create count in
  let state_of (module_ : Program.module_) =
    Hashtbl.find states module_.Program.module_id
  in
  let bases = ref [] in
  let start (module_ : Program.module_) =
    Hashtbl.add out.module_paths module_.Program.module_id module_.Program.path;
    List.iter
      (fun (unit_ : Program.unit_) ->
        bases :=
          (unit_.Program.source.Program.base, module_.Program.module_id)
          :: !bases)
      module_.Program.units;
    let is_root =
      module_.Program.module_id = program.Program.root.Program.module_id
    in
    Hashtbl.add states module_.Program.module_id
      (make_state ~out ~diags ~module_id:module_.Program.module_id
         ~module_path:module_.Program.path ~qualify:true ~is_root)
  in
  (* The scope is shared so an import sees names declared after this *)
  let failed_ids = Hashtbl.create count in
  Array.iter
    (fun (module_ : Program.module_) ->
      if module_.Program.failed then
        Hashtbl.replace failed_ids module_.Program.module_id ())
    program.Program.modules;
  let link_imports (module_ : Program.module_) =
    let st = state_of module_ in
    let bind (dependency : Program.dependency) =
      let target = Hashtbl.find states dependency.Program.target in
      let import = dependency.Program.import in
      match List.rev import.Ast.path with
      | [] -> ()
      | name :: _ -> (
          match Names.find_opt st.top.values name with
          | Some prev ->
              Diagnostic.emit st.diags
                (Diagnostic.redefinition import.Ast.span ~prev:prev.Symbol.span)
          | None ->
              Hashtbl.replace out.imports
                (module_.Program.module_id, [ name ])
                target.top;
              if Hashtbl.mem failed_ids dependency.Program.target then
                Hashtbl.replace out.failed_imports
                  (module_.Program.module_id, [ name ])
                  ();
              Names.replace st.top.values name
                (mint st Symbol.Module name import.Ast.span))
    in
    List.iter bind module_.Program.dependencies
  in
  let declare module_ =
    declare_decls (state_of module_) (Program.module_decls module_)
  in
  let resolve module_ =
    let st = state_of module_ in
    List.iter (resolve_decl st) (Program.module_decls module_)
  in
  Array.iter start program.Program.modules;
  out.file_modules <- Array.of_list !bases;
  Array.sort (fun (a, _) (b, _) -> compare a b) out.file_modules;
  Array.iter link_imports program.Program.modules;
  (* Every module declares before any body resolves so imports go both ways *)
  Array.iter declare program.Program.modules;
  Array.iter resolve program.Program.modules;
  let decls =
    program.Program.modules |> Array.to_list
    |> List.concat_map Program.module_decls
  in
  { uses = out; decls }
