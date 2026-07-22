(* SPDX-License-Identifier: GPL-2.0-only *)

open Types
module T = Typed_ast

(* the integers keep their real width so folding wraps like the runtime type *)
type const_num = Ni32 of Int32.t | Ni64 of Int64.t | Nf of float

let const_bool b = Ni32 (if b then 1l else 0l)

(* the source signedness says whether the new high bits are zeros or the sign bit *)
let const_to_int64 (src_ty : ty) (n : const_num) : Int64.t =
  match n with
  | Ni64 n -> n
  | Ni32 n ->
      if is_unsigned src_ty then Int64.logand (Int64.of_int32 n) 0xFFFFFFFFL
      else Int64.of_int32 n
  | Nf f -> Int64.of_float f

let const_to_float (n : const_num) : float =
  match n with
  | Ni32 n -> Int32.to_float n
  | Ni64 n -> Int64.to_float n
  | Nf f -> f

(* the narrow kinds get masked back to width so the value wraps like the target *)
let wrap_const (ty : ty) (n : Int64.t) : const_num =
  match resolve_ty ty with
  | TInt (I64 | U64 | Isize | Usize) -> Ni64 n
  | TInt kind ->
      let bits = int_kind_size kind * 8 in
      let fitted =
        if is_unsigned (TInt kind) then
          Int64.logand n (Int64.sub (Int64.shift_left 1L bits) 1L)
        else
          let shift = 64 - bits in
          Int64.shift_right (Int64.shift_left n shift) shift
      in
      Ni32 (Int64.to_int32 fitted)
  | _ -> if is_wide_ty ty then Ni64 n else Ni32 (Int64.to_int32 n)

let unsupported_const span =
  Diagnostic.(
    error "unsupported constant expression"
    |> at span
    |> help "constant initializers must fold to a compile-time value")

(* TODO(6676): once functions can be evaluated at compile time here bound the recursion depth and number of steps so a runaway evaluation cannot hang the compiler *)

(* [resolve] yields the value of a named constant so cycle handling stays in the caller *)
let rec fold_const_num ~sizeof
    ~(resolve : Symbol.t -> ty -> Ast.span -> const_num) (te : T.texpr) :
    const_num =
  let recur = fold_const_num ~sizeof ~resolve in
  match te.T.desc with
  | T.TInt n -> wrap_const te.T.ty n
  | T.TBool b -> const_bool b
  | T.TChar c -> Ni32 (Int32.of_int (Char.code c))
  | T.TFloat f -> Nf f
  | T.TSizeOf t -> wrap_const te.T.ty (Int64.of_int (sizeof t))
  | T.TIdent s -> (
      match resolve_ty te.T.ty with
      | TInt _ | TFloat _ | TBool -> resolve s te.T.ty te.T.span
      | _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ]))
  | T.TCast (e, _) -> (
      let v = recur e in
      match (resolve_ty te.T.ty, v) with
      | TFloat _, Nf f -> Nf f
      | TFloat _, _ -> Nf (const_to_float v)
      | _, Nf f -> wrap_const te.T.ty (Int64.of_float f)
      | _, _ -> wrap_const te.T.ty (const_to_int64 e.T.ty v))
  | T.TUnOp (Ast.Neg, e) -> (
      match recur e with
      | Nf f -> Nf (-.f)
      | v -> wrap_const te.T.ty (Int64.neg (const_to_int64 e.T.ty v)))
  | T.TUnOp (Ast.BitNot, e) -> (
      match recur e with
      | Nf _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])
      | v -> wrap_const te.T.ty (Int64.lognot (const_to_int64 e.T.ty v)))
  | T.TUnOp (Ast.Not, e) -> (
      match recur e with
      | Nf _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])
      | v -> const_bool (const_to_int64 e.T.ty v = 0L))
  | T.TUnOp ((Ast.Deref | Ast.AddressOf), _) ->
      raise (Diagnostic.Errors [ unsupported_const te.T.span ])
  | T.TBinOp (op, l, r) ->
      fold_const_binop te.T.span op ~result_ty:te.T.ty ~operand_ty:l.T.ty
        (recur l) (recur r)
  | _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])

and foldable (te : T.texpr) : bool = is_scalar te.T.ty && te.T.ty <> TError

and fold_const_binop (span : Ast.span) (op : Ast.binop) ~(result_ty : ty)
    ~(operand_ty : ty) (a : const_num) (b : const_num) : const_num =
  match (a, b) with
  | Nf _, _ | _, Nf _ -> (
      let x, y = (const_to_float a, const_to_float b) in
      match op with
      | Ast.Add -> Nf (x +. y)
      | Ast.Sub -> Nf (x -. y)
      | Ast.Mul -> Nf (x *. y)
      | Ast.Div -> Nf (x /. y)
      | Ast.Eq -> const_bool (x = y)
      | Ast.Neq -> const_bool (x <> y)
      | Ast.Lt -> const_bool (x < y)
      | Ast.Gt -> const_bool (x > y)
      | Ast.Lte -> const_bool (x <= y)
      | Ast.Gte -> const_bool (x >= y)
      | _ -> raise (Diagnostic.Errors [ unsupported_const span ]))
  | _ -> (
      let unsigned = is_unsigned operand_ty in
      let x = const_to_int64 operand_ty a and y = const_to_int64 operand_ty b in
      let wrap = wrap_const result_ty in
      let cmp =
        if unsigned then Int64.unsigned_compare x y else Int64.compare x y
      in
      match op with
      | Ast.Add -> wrap (Int64.add x y)
      | Ast.Sub -> wrap (Int64.sub x y)
      | Ast.Mul -> wrap (Int64.mul x y)
      | Ast.Div when y = 0L ->
          raise
            (Diagnostic.Errors
               [ Diagnostic.(error "division by zero in constant" |> at span) ])
      | Ast.Div ->
          wrap (if unsigned then Int64.unsigned_div x y else Int64.div x y)
      | Ast.Mod when y = 0L ->
          raise
            (Diagnostic.Errors
               [ Diagnostic.(error "remainder by zero in constant" |> at span) ])
      | Ast.Mod ->
          wrap (if unsigned then Int64.unsigned_rem x y else Int64.rem x y)
      | Ast.BitAnd -> wrap (Int64.logand x y)
      | Ast.BitOr -> wrap (Int64.logor x y)
      | Ast.BitXor -> wrap (Int64.logxor x y)
      (* the count is capped since ocaml leaves a shift past 64 bits undefined while go shifts every bit out *)
      | Ast.Lshift | Ast.Rshift -> (
          let oversized = Int64.unsigned_compare y 64L >= 0 in
          let n = Int64.to_int y in
          match op with
          | Ast.Lshift -> wrap (if oversized then 0L else Int64.shift_left x n)
          | _ when unsigned ->
              wrap (if oversized then 0L else Int64.shift_right_logical x n)
          | _ -> wrap (Int64.shift_right x (if oversized then 63 else n)))
      | Ast.Eq -> const_bool (x = y)
      | Ast.Neq -> const_bool (x <> y)
      | Ast.Lt -> const_bool (cmp < 0)
      | Ast.Gt -> const_bool (cmp > 0)
      | Ast.Lte -> const_bool (cmp <= 0)
      | Ast.Gte -> const_bool (cmp >= 0)
      | Ast.And -> const_bool (x <> 0L && y <> 0L)
      | Ast.Or -> const_bool (x <> 0L || y <> 0L)
      | Ast.Assign | Ast.AddAssign | Ast.SubAssign | Ast.MulAssign
      | Ast.DivAssign | Ast.ModAssign | Ast.BitAndAssign | Ast.BitOrAssign
      | Ast.BitXorAssign | Ast.LshiftAssign | Ast.RshiftAssign ->
          raise (Diagnostic.Errors [ unsupported_const span ]))
