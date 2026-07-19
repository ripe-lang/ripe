(* SPDX-License-Identifier: GPL-2.0-only *)

module S = Typed_ast
module D = Core

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
    | S.TBinOp (op, l, r) -> D.CBinOp (op, lower_expr l, lower_expr r)
    | S.TUnOp (op, e) -> D.CUnOp (op, lower_expr e)
    | S.TFieldAccess (e, name) -> D.CFieldAccess (lower_expr e, name)
    | S.TCast e -> D.CCast (lower_expr e)
    | S.TSizeOf t -> D.CSizeOf t
    | S.TRange (lo, hi) -> D.CRange (lower_expr lo, lower_expr hi)
    | S.TRangeInclusive (lo, hi) ->
        D.CRangeInclusive (lower_expr lo, lower_expr hi)
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
  in
  { D.desc; ty; span }

let rec lower_stmt (st : S.tstmt) : D.cstmt =
  let span = st.S.span in
  let tsdesc =
    match st.S.tsdesc with
    | S.TBinding (kind, sym, ty, e) -> D.CBinding (kind, sym, ty, lower_expr e)
    | S.TReturn e -> D.CReturn (Option.map lower_expr e)
    | S.TIf (branches, else_body) ->
        D.CIf
          ( List.map (fun (c, body) -> (lower_expr c, lower_stmts body)) branches,
            lower_stmts else_body )
    | S.TWhile (cond, body) -> D.CWhile (lower_expr cond, lower_stmts body)
    | S.TFor (sym, ty, iter, body) ->
        D.CFor (sym, ty, lower_expr iter, lower_stmts body)
    | S.TBreak -> D.CBreak
    | S.TContinue -> D.CContinue
    | S.TExpr e -> D.CExpr (lower_expr e)
    | S.TBlock body -> D.CBlock (lower_stmts body)
  in
  { D.tsdesc; span }

and lower_stmts (stmts : S.tstmt list) : D.cstmt list =
  List.map lower_stmt stmts

let lower_func (fd : S.tfunc_def) : D.cfunc_def =
  {
    D.name = fd.S.name;
    params = fd.S.params;
    ret_ty = fd.S.ret_ty;
    body = lower_stmts fd.S.body;
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

let lower (decls : S.tdecl list) : D.cdecl list = List.map lower_decl decls
