(* SPDX-License-Identifier: GPL-2.0-only *)

open Types
open Symbol
module T = Typed_ast

(* Every constant folds to a value here so codegen never resolves a name. the typechecker owns the const tables so lookups come in as closures *)
let run ~(emit : Diagnostic.t -> unit)
    ~(force_const : Ast.span -> Symbol.key -> unit)
    ~(local_value : Symbol.t -> Const_eval.const_num option)
    ~(global_value : Symbol.t -> Const_eval.const_num option)
    ~(fold_num : T.texpr -> Const_eval.const_num) (tdecls : T.tdecl list) :
    T.tdecl list =
  (* Fold every global const up front so an unused bad one still errors *)
  List.iter
    (function
      | T.TGlobal { T.key; init = Some init; kind = Ast.Comptime; _ } -> (
          try force_const init.T.span key
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

  (* A failed fold reports and hands back a default so checking continues *)
  let fold_num_or (default : Const_eval.const_num) (te : T.texpr) :
      Const_eval.const_num =
    if not (Const_eval.foldable te) then default
    else
      try fold_num te
      with Diagnostic.Errors ds ->
        List.iter emit ds;
        default
  in

  (* Function bodies stay runtime code and only const uses become literals *)
  let rec sub_expr (te : T.texpr) : T.texpr =
    let mk desc = { te with T.desc } in
    match te.T.desc with
    | T.TErrorExpr -> te
    | T.TIdent s when Symbol.is_comptime s.kind -> (
        match local_value s with Some v -> literal_of te v | None -> te)
    | T.TIdent s when Symbol.is_global s.kind -> (
        match global_value s with Some v -> literal_of te v | None -> te)
    | T.TInt _ | T.TFloat _ | T.TBool _ | T.TNull | T.TCStr _ | T.TStr _
    | T.TChar _ | T.TIdent _ | T.TSizeOf _ | T.TZero | T.TUndef ->
        te
    | T.TCall (callee, args, fixed) ->
        mk (T.TCall (sub_expr callee, List.map sub_expr args, fixed))
    | T.TBinOp (op, l, r) -> mk (T.TBinOp (op, sub_expr l, sub_expr r))
    | T.TUnOp (op, e) -> mk (T.TUnOp (op, sub_expr e))
    | T.TFieldAccess (e, f) -> mk (T.TFieldAccess (sub_expr e, f))
    | T.TCast (e, kind) -> mk (T.TCast (sub_expr e, kind))
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
    | T.TBlock body -> mk (T.TBlock (sub_block body))
    | T.TIf (branches, els) ->
        mk
          (T.TIf
             ( List.map (fun (c, body) -> (sub_expr c, sub_block body)) branches,
               Option.map sub_block els ))
    | T.TWhile (l, c, body) -> mk (T.TWhile (l, sub_expr c, sub_block body))
    | T.TFor (l, s, t, iter, body) ->
        mk (T.TFor (l, s, t, sub_expr iter, sub_block body))
    | T.TBinding (kind, s, t, e) -> mk (T.TBinding (kind, s, t, sub_expr e))
    | T.TReturn e -> mk (T.TReturn (Option.map sub_expr e))
    | T.TBreak _ | T.TContinue _ -> te
    | T.TPairAssign (ft, st, fv, sv) ->
        mk (T.TPairAssign (sub_expr ft, sub_expr st, sub_expr fv, sub_expr sv))
    | T.TLocalDecl -> te
  (* A const binding folded while checking so it just vanishes from the block *)
  and sub_block (body : T.tblock) : T.tblock =
    List.filter_map
      (fun e ->
        match e.T.desc with
        | T.TBinding (Ast.Comptime, _, _, _) -> None
        | _ -> Some (sub_expr e))
      body
  in

  (* QBE data needs scalar literals while aggregates keep their shape *)
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

  (* A const global vanishes and every other global keeps folded data *)
  List.filter_map
    (function
      | T.TGlobal { T.kind = Ast.Comptime; _ } -> None
      | T.TGlobal gd ->
          let init =
            match gd.T.init with
            | Some init when is_scalar gd.T.ty && init.T.ty <> TError ->
                Some (literal_of init (fold_num_or (Const_eval.Ni32 0l) init))
            | Some init -> Some (fold_global_init init)
            | None -> None
          in
          Some (T.TGlobal { gd with T.init })
      | T.TFunc fd -> Some (T.TFunc { fd with T.body = sub_block fd.T.body })
      | d -> Some d)
    tdecls
