(* SPDX-License-Identifier: GPL-2.0-only *)

open Types
module T = Typed_ast

(* every constant folds to a value here so codegen never resolves a name.
   the typechecker owns the const tables so lookups come in as closures *)
let run ~(emit : Diagnostic.t -> unit)
    ~(force_const : Ast.span -> string -> unit)
    ~(local_value : Symbol.id -> Const_eval.const_num option)
    ~(global_value : string -> Const_eval.const_num option)
    ~(fold_num : T.texpr -> Const_eval.const_num) (tdecls : T.tdecl list) :
    T.tdecl list =
  (* fold every global const up front so an unused bad one still errors *)
  List.iter
    (function
      | T.TGlobal { T.name; init = Some init; kind = Ast.Const; _ } -> (
          try force_const init.T.span name
          with Diagnostic.Errors ds -> List.iter emit ds)
      | _ -> ())
    tdecls;

  let literal_of (te : T.texpr) (v : Const_eval.const_num) : T.texpr =
    let desc =
      match (resolve_ty te.T.ty, v) with
      | TBool, Const_eval.Ni32 n -> T.TBool (n <> 0l)
      | _, Const_eval.Ni32 n -> T.TInt (Int64.of_int32 n)
      | _, Const_eval.Ni64 n -> T.TInt n
      | _, Const_eval.Nf f -> T.TFloat f
    in
    { te with T.desc }
  in

  (* a failed fold reports and hands back a default so checking continues *)
  let fold_num_or (default : Const_eval.const_num) (te : T.texpr) :
      Const_eval.const_num =
    if not (Const_eval.foldable te) then default
    else
      try fold_num te
      with Diagnostic.Errors ds ->
        List.iter emit ds;
        default
  in

  (* function bodies stay runtime code and only const uses become literals *)
  let rec sub_expr (te : T.texpr) : T.texpr =
    let mk desc = { te with T.desc } in
    match te.T.desc with
    | T.TIdent s when Symbol.is_comptime s.kind -> (
        match local_value s.id with Some v -> literal_of te v | None -> te)
    | T.TIdent s when Symbol.is_global s.kind -> (
        match global_value s.name with Some v -> literal_of te v | None -> te)
    | T.TInt _ | T.TFloat _ | T.TBool _ | T.TNull | T.TCStr _ | T.TChar _
    | T.TIdent _ | T.TSizeOf _ | T.TZero | T.TUndef ->
        te
    | T.TCall (callee, args, fixed) ->
        mk (T.TCall (sub_expr callee, List.map sub_expr args, fixed))
    | T.TBinOp (op, l, r) -> mk (T.TBinOp (op, sub_expr l, sub_expr r))
    | T.TUnOp (op, e) -> mk (T.TUnOp (op, sub_expr e))
    | T.TFieldAccess (e, f) -> mk (T.TFieldAccess (sub_expr e, f))
    | T.TCast (e, checked) -> mk (T.TCast (sub_expr e, checked))
    | T.TRange (lo, hi) -> mk (T.TRange (sub_expr lo, sub_expr hi))
    | T.TRangeInclusive (lo, hi) ->
        mk (T.TRangeInclusive (sub_expr lo, sub_expr hi))
    | T.TArrayLit elems -> mk (T.TArrayLit (List.map sub_expr elems))
    | T.TIndex (base, idx) -> mk (T.TIndex (sub_expr base, sub_expr idx))
    | T.TLen e -> mk (T.TLen (sub_expr e))
    | T.TToSlice e -> mk (T.TToSlice (sub_expr e))
    | T.TSliceExpr (base, lo, hi) ->
        mk (T.TSliceExpr (sub_expr base, sub_expr lo, sub_expr hi))
    | T.TDataPtr e -> mk (T.TDataPtr (sub_expr e))
    | T.TStructLit (name, fields) ->
        mk
          (T.TStructLit (name, List.map (fun (f, e) -> (f, sub_expr e)) fields))
    | T.TBlockExpr (body, e) -> mk (T.TBlockExpr (sub_stmts body, sub_expr e))
  (* a const binding folded while checking so it just vanishes from the tree *)
  and sub_stmt (st : T.tstmt) : T.tstmt option =
    let keep tsdesc = Some { st with T.tsdesc } in
    match st.T.tsdesc with
    | T.TBinding (Ast.Const, _, _, _) -> None
    | T.TBinding (kind, s, t, e) -> keep (T.TBinding (kind, s, t, sub_expr e))
    | T.TReturn e -> keep (T.TReturn (Option.map sub_expr e))
    | T.TIf (branches, els) ->
        keep
          (T.TIf
             ( List.map (fun (c, body) -> (sub_expr c, sub_stmts body)) branches,
               sub_stmts els ))
    | T.TWhile (c, body) -> keep (T.TWhile (sub_expr c, sub_stmts body))
    | T.TFor (s, t, iter, body) ->
        keep (T.TFor (s, t, sub_expr iter, sub_stmts body))
    | (T.TBreak | T.TContinue) as d -> keep d
    | T.TExpr e -> keep (T.TExpr (sub_expr e))
    | T.TBlock body -> keep (T.TBlock (sub_stmts body))
  and sub_stmts (body : T.tstmt list) : T.tstmt list =
    List.filter_map sub_stmt body
  in

  (* scalars must be finished values for QBE data and the rest stays symbolic *)
  let fold_scalar (te : T.texpr) : T.texpr =
    match te.T.desc with
    | T.TInt _ | T.TFloat _ | T.TBool _ -> te
    | _ when Const_eval.foldable te -> (
        try literal_of te (fold_num te)
        with Diagnostic.Errors ds ->
          List.iter emit ds;
          te)
    | _ -> te
  in
  let rec fold_global_init (te : T.texpr) : T.texpr =
    match te.T.desc with
    | T.TArrayLit elems ->
        { te with T.desc = T.TArrayLit (List.map fold_global_init elems) }
    | T.TStructLit (name, fields) ->
        {
          te with
          T.desc =
            T.TStructLit
              (name, List.map (fun (f, e) -> (f, fold_global_init e)) fields);
        }
    | _ -> fold_scalar te
  in

  (* a const global vanishes and every other global keeps folded data *)
  List.filter_map
    (function
      | T.TGlobal { T.kind = Ast.Const; _ } -> None
      | T.TGlobal gd ->
          let init =
            match gd.T.init with
            | Some init when is_scalar gd.T.ty && init.T.ty <> TError ->
                Some (literal_of init (fold_num_or (Const_eval.Ni32 0l) init))
            | Some init -> Some (fold_global_init init)
            | None -> None
          in
          Some (T.TGlobal { gd with T.init })
      | T.TFunc fd -> Some (T.TFunc { fd with T.body = sub_stmts fd.T.body })
      | d -> Some d)
    tdecls
