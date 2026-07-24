(* SPDX-License-Identifier: GPL-2.0-only *)

module S = Typed_ast
module D = Core

(* negative ids never clash with the resolver's, which count up from zero *)
let sym_counter = ref 0

let fresh_sym name : Symbol.t =
  let c = !sym_counter in
  incr sym_counter;
  {
    Symbol.id = -1 - c;
    name = Printf.sprintf "%s.%d" name c;
    kind = Symbol.Local Ast.Var;
    span = Ast.dummy_span;
  }

let voidc (desc : D.cexpr_desc) : D.cexpr = D.mk Types.TVoid desc
let neverc (desc : D.cexpr_desc) : D.cexpr = D.mk Types.TNever desc
let ident ty sym = D.mk ty (D.CIdent sym)
let int ty n = D.mk ty (D.CInt n)
let binop ty op a b = D.mk ty (D.CBinOp (op, a, b))
let bind sym ty e = voidc (D.CBinding (Ast.Var, sym, ty, e))
let assign (lhs : D.cexpr) rhs = binop lhs.D.ty Ast.Assign lhs rhs
let if_then cond body = voidc (D.CIf ([ (cond, body) ], None))

(* a for-loop continue still has to run the step, so drop a copy in front of each one *)
(* nested loops aren't touched here since their continues belong to them *)
let rec paste_step step (stmts : D.cblock) : D.cblock =
  let on_stmt (st : D.cexpr) =
    match st.D.desc with
    | D.CContinue -> step @ [ st ]
    (* a nested loop owns its own continues *)
    | D.CLoop _ -> [ st ]
    | D.CReturn (Some e) ->
        [ { st with D.desc = D.CReturn (Some (paste_in_expr step e)) } ]
    | D.CIf _ | D.CBlock _ | D.CBinding _ -> [ paste_in_expr step st ]
    | _ -> [ st ]
  in
  List.concat_map on_stmt stmts

(* a continue can hide in a value if that a binding or return holds *)
and paste_in_expr step (e : D.cexpr) : D.cexpr =
  match e.D.desc with
  | D.CIf (branches, else_body) ->
      let branches =
        List.map (fun (c, body) -> (c, paste_step step body)) branches
      in
      {
        e with
        D.desc = D.CIf (branches, Option.map (paste_step step) else_body);
      }
  | D.CBlock body -> { e with D.desc = D.CBlock (paste_step step body) }
  | D.CBinding (k, s, t, init) ->
      { e with D.desc = D.CBinding (k, s, t, paste_in_expr step init) }
  | _ -> e

let loop ~init ~cond ~step ~body : D.cblock =
  let not_cond = D.mk Types.TBool (D.CUnOp (Ast.Not, cond)) in
  let guard = if_then not_cond [ neverc D.CBreak ] in
  let body = if step = [] then body else paste_step step body @ step in
  let bare = voidc (D.CLoop (guard :: body)) in
  init @ [ bare ]

let rec lower_expr (te : S.texpr) : D.cexpr =
  let ty = te.S.ty and span = te.S.span in
  let desc =
    match te.S.desc with
    | S.TInt n -> D.CInt n
    | S.TFloat f -> D.CFloat f
    | S.TBool b -> D.CBool b
    | S.TNull -> D.CNull
    | S.TCStr s -> D.CCStr s
    | S.TChar c -> D.CChar c
    | S.TIdent sym -> D.CIdent sym
    | S.TCall (f, args, variadic_start) ->
        D.CCall (lower_expr f, List.map lower_expr args, variadic_start)
    | S.TBinOp (Ast.And, l, r) ->
        D.CIf
          ( [ (lower_expr l, [ lower_expr r ]) ],
            Some [ D.mk ~span ty (D.CBool false) ] )
    | S.TBinOp (Ast.Or, l, r) ->
        D.CIf
          ( [ (lower_expr l, [ D.mk ~span ty (D.CBool true) ]) ],
            Some [ lower_expr r ] )
    | S.TBinOp (op, l, r) -> D.CBinOp (op, lower_expr l, lower_expr r)
    | S.TUnOp (op, e) -> D.CUnOp (op, lower_expr e)
    | S.TFieldAccess (e, name) -> D.CFieldAccess (lower_expr e, name)
    | S.TCast (e, checked) -> D.CCast (lower_expr e, checked)
    | S.TSizeOf t -> D.CSizeOf t
    | S.TRange _ | S.TRangeInclusive _ ->
        Error.ice ~span "range outside a for loop"
    | S.TArrayLit es -> D.CArrayLit (List.map lower_expr es)
    | S.TIndex (base, idx) -> D.CIndex (lower_expr base, lower_expr idx)
    | S.TLen e -> D.CLen (lower_expr e)
    | S.TToSlice e -> D.CToSlice (lower_expr e)
    | S.TSliceExpr (base, lo, hi) ->
        D.CSliceExpr (lower_expr base, lower_expr lo, lower_expr hi)
    | S.TDataPtr e -> D.CDataPtr (lower_expr e)
    | S.TZero -> D.CZero
    | S.TUndef -> D.CUndef
    | S.TStructLit (name, fields) ->
        D.CStructLit (name, List.map (fun (n, e) -> (n, lower_expr e)) fields)
    | S.TBlock body -> D.CBlock (lower_block body)
    | S.TIf (branches, else_body) ->
        D.CIf
          ( List.map (fun (c, body) -> (lower_expr c, lower_block body)) branches,
            Option.map lower_block else_body )
    (* while and for are void so they only reach here from an unused value
       slot *)
    | S.TWhile (cond, body) -> D.CBlock (lower_while cond body)
    | S.TFor (sym, elem_ty, iter, body) ->
        D.CBlock (lower_for sym elem_ty iter body)
    | S.TBinding (kind, sym, t, e) -> D.CBinding (kind, sym, t, lower_expr e)
    | S.TReturn e -> D.CReturn (Option.map lower_expr e)
    | S.TBreak -> D.CBreak
    | S.TContinue -> D.CContinue
  in
  { D.desc; ty; span }

(* a block element in statement position may expand to several core
   statements *)
and lower_block (body : S.tblock) : D.cblock = List.concat_map lower_elem body

and lower_elem (te : S.texpr) : D.cblock =
  match te.S.desc with
  | S.TWhile (cond, body) -> lower_while cond body
  | S.TFor (sym, elem_ty, iter, body) -> lower_for sym elem_ty iter body
  | S.TBinOp (op, l, r) when base_binop_of op <> None ->
      lower_compound_assign op l r
  | _ -> [ lower_expr te ]

and lower_while cond body : D.cblock =
  loop ~init:[] ~cond:(lower_expr cond) ~step:[] ~body:(lower_block body)

(* x op= r runs as x = x op r, and likewise for every other compound form *)
and base_binop_of = function
  | Ast.AddAssign -> Some Ast.Add
  | Ast.SubAssign -> Some Ast.Sub
  | Ast.MulAssign -> Some Ast.Mul
  | Ast.DivAssign -> Some Ast.Div
  | Ast.ModAssign -> Some Ast.Mod
  | Ast.BitAndAssign -> Some Ast.BitAnd
  | Ast.BitOrAssign -> Some Ast.BitOr
  | Ast.BitXorAssign -> Some Ast.BitXor
  | Ast.LshiftAssign -> Some Ast.Lshift
  | Ast.RshiftAssign -> Some Ast.Rshift
  | _ -> None

(* the target's address is taken once so an index or base with side effects
   doesn't run twice *)
and lower_compound_assign op (l : S.texpr) (r : S.texpr) : D.cblock =
  let elem = l.S.ty in
  let ptr_ty = Types.TPointer elem in
  let span = l.S.span in
  let base =
    match base_binop_of op with
    | Some b -> b
    | None -> Error.ice ~span "expected a compound assignment operator"
  in
  let psym = fresh_sym "compound.p" in
  let pvar = ident ptr_ty psym in
  let place = D.mk ~span elem (D.CUnOp (Ast.Deref, pvar)) in
  let addr = D.mk ~span ptr_ty (D.CUnOp (Ast.AddressOf, lower_expr l)) in
  let updated = binop elem base place (lower_expr r) in
  [ bind psym ptr_ty addr; assign place updated ]

and lower_range_for sym elem_ty lo hi ~inclusive body : D.cblock =
  let ivar = ident elem_ty sym in
  let hisym = fresh_sym "for.hi" in
  let hivar = ident elem_ty hisym in
  let init = [ bind sym elem_ty lo; bind hisym elem_ty hi ] in
  let cond =
    binop Types.TBool (if inclusive then Ast.Lte else Ast.Lt) ivar hivar
  in
  let incr = assign ivar (binop elem_ty Ast.Add ivar (int elem_ty 1L)) in
  (* the guard keeps an inclusive range from incrementing past the type's max *)
  let step =
    if inclusive then
      [
        if_then (binop Types.TBool Ast.Eq ivar hivar) [ neverc D.CBreak ]; incr;
      ]
    else [ incr ]
  in
  loop ~init ~cond ~step ~body

and lower_each_for sym elem_ty (iter : D.cexpr) body : D.cblock =
  let usize = Types.TInt Types.Usize in
  let ptr_ty = Types.TPointer elem_ty in
  (* a slice is snapshotted so its pointer and length come from one
     evaluation *)
  let pre, src =
    match Types.resolve_ty iter.D.ty with
    | Types.TSlice _ ->
        let it = fresh_sym "for.it" in
        ([ bind it iter.D.ty iter ], ident iter.D.ty it)
    | _ -> ([], iter)
  in
  let psym = fresh_sym "for.p" in
  let nsym = fresh_sym "for.n" in
  let isym = fresh_sym "for.i" in
  let ivar = ident usize isym in
  let init =
    pre
    @ [
        bind psym ptr_ty (D.mk ptr_ty (D.CDataPtr src));
        bind nsym usize (D.mk usize (D.CLen src));
        bind isym usize (int usize 0L);
      ]
  in
  let cond = binop Types.TBool Ast.Lt ivar (ident usize nsym) in
  let elem = D.mk elem_ty (D.CIndex (ident ptr_ty psym, ivar)) in
  let incr = assign ivar (binop usize Ast.Add ivar (int usize 1L)) in
  let body = bind sym elem_ty elem :: body in
  loop ~init ~cond ~step:[ incr ] ~body

and lower_for sym elem_ty iter body : D.cblock =
  match iter.S.desc with
  | S.TRange (lo, hi) ->
      lower_range_for sym elem_ty (lower_expr lo) (lower_expr hi)
        ~inclusive:false (lower_block body)
  | S.TRangeInclusive (lo, hi) ->
      lower_range_for sym elem_ty (lower_expr lo) (lower_expr hi)
        ~inclusive:true (lower_block body)
  | _ -> lower_each_for sym elem_ty (lower_expr iter) (lower_block body)

let lower_func (fd : S.tfunc_def) : D.cfunc_def =
  {
    D.name = fd.S.name;
    params = fd.S.params;
    ret_ty = fd.S.ret_ty;
    body = lower_block fd.S.body;
    modifiers = fd.S.modifiers;
    variadic = fd.S.variadic;
  }

let lower_global (g : S.tglobal_def) : D.cglobal_def =
  {
    D.name = g.S.name;
    ty = g.S.ty;
    init = Option.map lower_expr g.S.init;
    kind = g.S.kind;
  }

let lower_decl (d : S.tdecl) : D.cdecl =
  match d with
  | S.TFunc fd -> D.CFunc (lower_func fd)
  | S.TStruct (name, fields, mods) -> D.CStruct (name, fields, mods)
  | S.TExtern fd -> D.CExtern (lower_func fd)
  | S.TGlobal g -> D.CGlobal (lower_global g)
  | S.TTypeAlias (name, ty) -> D.CTypeAlias (name, ty)
  | S.TNewtype (name, ty) -> D.CNewtype (name, ty)

let lower (decls : S.tdecl list) : D.cdecl list =
  sym_counter := 0;
  List.map lower_decl decls
