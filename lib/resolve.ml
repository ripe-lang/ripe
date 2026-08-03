(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast

type t = {
  syms : (Ast.span, Symbol.t) Hashtbl.t;
  module_paths : (Symbol.module_id, string list) Hashtbl.t;
  file_modules : (Span.file_id, Symbol.module_id) Hashtbl.t;
}

type namespace = {
  values : (string, Symbol.t) Hashtbl.t;
  types : (string, Symbol.t) Hashtbl.t;
}

type state = {
  out : t;
  module_id : Symbol.module_id;
  module_path : string list;
  qualify : bool;
  is_root : bool;
  globals : (string, Symbol.t) Hashtbl.t;
  types : (string, Symbol.t) Hashtbl.t;
  mutable imports : (string list * namespace) list;
  mutable scopes : (string, Symbol.t) Hashtbl.t list;
  mutable next_id : Symbol.id;
  diags : Diagnostic.sink;
}

type resolved_program = { uses : t; decls : Ast.decl list }
type qualified = Local | Found of Symbol.t | Missing of string

(* This is the `--emit resolve` output *)
let dump (r : t) : string =
  Hashtbl.to_seq r.syms |> List.of_seq
  |> List.sort (fun ((a : Ast.span), _) ((b : Ast.span), _) ->
      compare (a.file, a.lo, a.hi) (b.file, b.lo, b.hi))
  |> List.map (fun ((sp : Ast.span), (s : Symbol.t)) ->
      Printf.sprintf "(%d,%d) -> #%d %s %s\n" sp.Ast.lo sp.Ast.hi s.Symbol.id
        (Symbol.show_kind s.Symbol.kind)
        s.Symbol.name)
  |> String.concat ""

let sym_at (r : t) (span : Ast.span) : Symbol.t =
  match Hashtbl.find_opt r.syms span with
  | Some s -> s
  | None -> Error.ice ~span "no symbol resolved here"

let sym_at_opt (r : t) (span : Ast.span) : Symbol.t option =
  Hashtbl.find_opt r.syms span

let qname_of (r : t) (s : Symbol.t) : Qname.t =
  let path = Hashtbl.find r.module_paths s.Symbol.module_id in
  Qname.make (Symbol.key s) path s.Symbol.name

(* A message inside math says add where one outside says math.add *)
let module_path_at (r : t) (span : Ast.span) : string list =
  match Hashtbl.find_opt r.file_modules span.Ast.file with
  | None -> []
  | Some module_id -> (
      match Hashtbl.find_opt r.module_paths module_id with
      | Some path -> path
      | None -> [])

let is_entry (st : state) (kind : Symbol.kind) (name : string) : bool =
  kind = Symbol.Func && st.is_root && name = "main"

(* An extern and the entry point are named by something outside the compiler *)
let declaration_link_name (st : state) (kind : Symbol.kind) (name : string) :
    string =
  if not st.qualify then name
  else if is_entry st kind name then name
  else
    match kind with
    | Symbol.Extern -> name
    | _ -> Mangle.declaration st.module_path name

let mint ?(visibility = Symbol.Private) ?link_name (st : state)
    (kind : Symbol.kind) (name : string) (span : Ast.span) : Symbol.t =
  let id = st.next_id in
  st.next_id <- id + 1;
  let link_name = Option.value ~default:name link_name in
  let sym =
    {
      Symbol.id;
      module_id = st.module_id;
      name;
      link_name;
      kind;
      visibility;
      entry_point = is_entry st kind name;
      span;
    }
  in
  Hashtbl.replace st.out.syms span sym;
  sym

let push_scope (st : state) = st.scopes <- Hashtbl.create 8 :: st.scopes

let pop_scope (st : state) =
  st.scopes <-
    (match st.scopes with _ :: scopes -> scopes | [] -> assert false)

(* The innermost scope holds params and top level body binders *)
let declare_local (st : state) kind name span : unit =
  let sym = mint st kind name span in
  match st.scopes with
  | scope :: _ -> Hashtbl.replace scope name sym
  | [] -> assert false

let declare_global (st : state) kind visibility name span : unit =
  match Hashtbl.find_opt st.globals name with
  | Some prev ->
      Diagnostic.emit st.diags
        (Error.redefinition span ~prev:prev.Symbol.span name)
  | None ->
      let link_name = declaration_link_name st kind name in
      let sym = mint ~visibility ~link_name st kind name span in
      Hashtbl.replace st.globals name sym

let declare_type (st : state) visibility name span : unit =
  match Hashtbl.find_opt st.types name with
  | Some prev ->
      Diagnostic.emit st.diags
        (Error.redefinition span ~prev:prev.Symbol.span name)
  | None ->
      let link_name = declaration_link_name st Symbol.Type name in
      Hashtbl.replace st.types name
        (mint ~visibility ~link_name st Symbol.Type name span)

let lookup (st : state) (name : string) : Symbol.t option =
  match List.find_map (fun scope -> Hashtbl.find_opt scope name) st.scopes with
  | Some s -> Some s
  | None -> Hashtbl.find_opt st.globals name

(* The name comes off the symbol not off the spelling at the use site *)
let check_visibility (st : state) (span : Ast.span) (sym : Symbol.t) : unit =
  if
    sym.Symbol.module_id <> st.module_id
    && sym.Symbol.visibility = Symbol.Private
  then
    let shown = Qname.show_in st.module_path (qname_of st.out sym) in
    Diagnostic.emit st.diags
      (Error.named span "private declaration" shown
      |> Diagnostic.secondary sym.Symbol.span "declared private here")

let use_symbol (st : state) (span : Ast.span) (sym : Symbol.t) : unit =
  check_visibility st span sym;
  Hashtbl.replace st.out.syms span sym

let imported_namespace (st : state) (path : string list) : namespace option =
  List.assoc_opt path st.imports

let split_member (path : string list) : (string list * string) option =
  match List.rev path with
  | member :: rest -> Some (List.rev rest, member)
  | [] -> None

(* math.Vec goes through the import and Vec stays in this module *)
let find_type (st : state) (path : string list) (name : string) :
    Symbol.t option =
  if path = [] then Hashtbl.find_opt st.types name
  else
    match imported_namespace st path with
    | Some namespace -> Hashtbl.find_opt namespace.types name
    | None -> None

let use_type (st : state) (path : string list) (name : string) (span : Ast.span)
    : unit =
  let shown = Ast.show_named path name in
  if not (path = [] && List.mem_assoc name Types.builtin_tys) then
    match find_type st path name with
    | Some sym -> use_symbol st span sym
    | None ->
        Diagnostic.emit st.diags (Error.undefined_name span "type" shown);
        ignore (mint st Symbol.Error shown span)

(* The typechecker already reports an unknown struct literal *)
let use_type_if_found (st : state) (name : string) (span : Ast.span) : unit =
  match find_type st [] name with
  | Some sym -> use_symbol st span sym
  | None -> ()

let use (st : state) ~(what : string) name span : unit =
  match lookup st name with
  | Some sym -> Hashtbl.replace st.out.syms span sym
  | None ->
      Diagnostic.emit st.diags (Error.undefined_name span what name);
      ignore (mint st Symbol.Error name span)

let rec access_path (e : expr) : string list option =
  match e.desc with
  | Ident name -> Some [ name ]
  | FieldAccess (inner, name) ->
      Option.map (fun path -> path @ [ name ]) (access_path inner)
  | _ -> None

let root_in_scope (st : state) (path : string list) : bool =
  match path with root :: _ -> lookup st root <> None | [] -> true

(* A local named math wins so math.x stays an ordinary field access *)
let qualified_use (st : state) (e : expr) : qualified =
  match Option.bind (access_path e) split_member with
  | Some (module_path, member) when not (root_in_scope st module_path) -> (
      match imported_namespace st module_path with
      | None -> Local
      | Some namespace -> (
          match Hashtbl.find_opt namespace.values member with
          | Some sym -> Found sym
          | None -> Missing (String.concat "." (module_path @ [ member ]))))
  | _ -> Local

let use_qualified (st : state) ~(what : string) (e : expr) : bool =
  match qualified_use st e with
  | Local -> false
  | Found sym ->
      use_symbol st e.span sym;
      true
  | Missing name ->
      Diagnostic.emit st.diags (Error.undefined_name e.span what name);
      (* The stages after this read a symbol back off every span they walk *)
      ignore (mint st Symbol.Error name e.span);
      true

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
  | FieldAccess (inner, _) ->
      if not (use_qualified st ~what:"variable" e) then resolve_expr st inner
  | Cast (inner, ty, _) ->
      resolve_expr st inner;
      resolve_typ st ty
  | SizeOf ty -> resolve_typ st ty
  | Index (base, idx) ->
      resolve_expr st base;
      resolve_expr st idx
  | ArrayLit elems -> List.iter (resolve_expr st) elems
  | StructLit (name, name_span, fields) ->
      use_type_if_found st name name_span;
      List.iter (fun (_, _, e) -> resolve_expr st e) fields
  | Block body -> resolve_block st body
  | If (branches, else_body) ->
      List.iter
        (fun (cond, body) ->
          resolve_expr st cond;
          resolve_block st body)
        branches;
      Option.iter (resolve_block st) else_body
  | While (cond, body) ->
      resolve_expr st cond;
      resolve_block st body
  | For (name, nspan, iter, body) ->
      resolve_expr st iter;
      push_scope st;
      declare_local st Symbol.ForVar name nspan;
      List.iter (resolve_expr st) body;
      pop_scope st
  | Binding (kind, name, nspan, ann, e) ->
      Option.iter (resolve_typ st) ann;
      Option.iter (resolve_expr st) e;
      declare_local st (Symbol.Local kind) name nspan
  | Return e -> Option.iter (resolve_expr st) e
  | Break | Continue -> ()
  | Int _ | Float _ | Bool _ | Null | Char _ | String _ | Undefined -> ()
  | PairAssign (ft, st', fv, sv) ->
      resolve_expr st ft;
      resolve_expr st st';
      resolve_expr st fv;
      resolve_expr st sv

(* An array size expression may name constants *)
and resolve_typ (st : state) (t : typ) : unit =
  match t.tdesc with
  | ErrorType -> ()
  | Named ([], "opaque") -> ()
  | Named (path, name) -> use_type st path name t.tspan
  | Pointer t | Slice t -> resolve_typ st t
  | Array (e, t) ->
      resolve_expr st e;
      resolve_typ st t
  | FuncPtr (ps, ret) ->
      List.iter (resolve_typ st) ps;
      Option.iter (resolve_typ st) ret

and resolve_block (st : state) (body : block) : unit =
  push_scope st;
  List.iter (resolve_expr st) body;
  pop_scope st

(* Body binders can redeclare but params can't repeat *)
let declare_param (st : state) (p : param) : unit =
  let scope = match st.scopes with scope :: _ -> scope | [] -> assert false in
  match Hashtbl.find_opt scope p.name with
  | Some prev ->
      Diagnostic.emit st.diags
        (Error.redefinition p.span ~prev:prev.Symbol.span p.name);
      Hashtbl.replace st.out.syms p.span prev
  | None -> declare_local st Symbol.Param p.name p.span

let resolve_func (st : state) (fd : func_def) : unit =
  push_scope st;
  List.iter
    (fun (p : param) ->
      resolve_typ st p.typ;
      declare_param st p)
    fd.params;
  Option.iter (resolve_typ st) fd.ret;
  List.iter (resolve_expr st) fd.body;
  pop_scope st

let visibility modifiers =
  if List.mem Ast.Pub modifiers then Symbol.Public else Symbol.Private

let make_state ~(out : t) ~(diags : Diagnostic.sink)
    ~(module_id : Symbol.module_id) ~(module_path : string list)
    ~(qualify : bool) ~(is_root : bool) : state =
  {
    out;
    module_id;
    module_path;
    qualify;
    is_root;
    globals = Hashtbl.create 64;
    types = Hashtbl.create 64;
    imports = [];
    scopes = [];
    next_id = 0;
    diags;
  }

(* A top level name lands first so a body can forward reference *)
let declare_decls (st : state) (decls : decl list) : unit =
  List.iter
    (function
      | Func fd ->
          declare_global st Symbol.Func (visibility fd.modifiers) fd.name
            fd.span
      | Extern fd ->
          declare_global st Symbol.Extern Symbol.Private fd.name fd.span
      | Global gd ->
          declare_global st Symbol.Global Symbol.Private gd.name gd.span
      | Struct sd -> declare_type st (visibility sd.modifiers) sd.name sd.span
      | TypeAlias td | Newtype td ->
          declare_type st Symbol.Private td.name td.span)
    decls

let resolve_decls (st : state) (decls : decl list) : unit =
  List.iter
    (function
      | Func fd | Extern fd -> resolve_func st fd
      | Global gd ->
          resolve_typ st gd.typ;
          Option.iter (resolve_expr st) gd.init
      | Struct sd ->
          List.iter (fun (f : field) -> resolve_typ st f.typ) sd.fields
      | TypeAlias td | Newtype td -> resolve_typ st td.typ)
    decls

let resolve ~(diags : Diagnostic.sink) ~(module_id : Symbol.module_id)
    (decls : decl list) : t =
  let out =
    {
      syms = Hashtbl.create 256;
      module_paths = Hashtbl.create 1;
      file_modules = Hashtbl.create 1;
    }
  in
  Hashtbl.add out.module_paths module_id [];
  let st =
    make_state ~out ~diags ~module_id ~module_path:[] ~qualify:false
      ~is_root:true
  in
  declare_decls st decls;
  resolve_decls st decls;
  out

let resolve_program ~(diags : Diagnostic.sink) (program : Program.t) :
    resolved_program =
  let count = Array.length program.Program.modules in
  let out =
    {
      syms = Hashtbl.create 512;
      module_paths = Hashtbl.create count;
      file_modules = Hashtbl.create count;
    }
  in
  let states = Hashtbl.create count in
  let state_of (module_ : Program.module_) =
    Hashtbl.find states module_.Program.module_id
  in
  let start (module_ : Program.module_) =
    Hashtbl.add out.module_paths module_.Program.module_id module_.Program.path;
    List.iter
      (fun (unit_ : Program.unit_) ->
        Hashtbl.replace out.file_modules unit_.Program.source.Program.file_id
          module_.Program.module_id)
      module_.Program.units;
    let is_root =
      module_.Program.module_id = program.Program.root.Program.module_id
    in
    Hashtbl.add states module_.Program.module_id
      (make_state ~out ~diags ~module_id:module_.Program.module_id
         ~module_path:module_.Program.path ~qualify:true ~is_root)
  in
  (* Both tables are shared so an import sees names declared after this *)
  let link_imports (module_ : Program.module_) =
    let namespace_of (dependency : Program.dependency) =
      let target = Hashtbl.find states dependency.Program.target in
      ( dependency.Program.import.Ast.path,
        { values = target.globals; types = target.types } )
    in
    (state_of module_).imports <-
      List.map namespace_of module_.Program.dependencies
  in
  let declare module_ =
    declare_decls (state_of module_) (Program.module_decls module_)
  in
  let resolve module_ =
    resolve_decls (state_of module_) (Program.module_decls module_)
  in
  Array.iter start program.Program.modules;
  Array.iter link_imports program.Program.modules;
  (* Every module declares before any body resolves so imports go both ways *)
  Array.iter declare program.Program.modules;
  Array.iter resolve program.Program.modules;
  let decls =
    program.Program.modules |> Array.to_list
    |> List.concat_map Program.module_decls
  in
  { uses = out; decls }
