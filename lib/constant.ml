(* SPDX-License-Identifier: Apache-2.0 *)

open Types

exception Overflow

type exact = { magnitude : int64; neg : bool }

type value =
  | VInt of exact * int_kind
  | VFloat of float * float_kind
  | VBool of bool
  | VChar of int

let zero = { magnitude = 0L; neg = false }

let of_magnitude ?(neg = false) magnitude =
  if magnitude = 0L then zero else { magnitude; neg }

let of_bits n =
  if Int64.compare n 0L < 0 then of_magnitude ~neg:true (Int64.neg n)
  else of_magnitude n

let bits_of e = if e.neg then Int64.neg e.magnitude else e.magnitude
let one = of_magnitude 1L
let negate e = of_magnitude ~neg:(not e.neg) e.magnitude

(* A big one top bit set so reading it signed would flip it negative *)
let is_big e = (not e.neg) && Int64.compare e.magnitude 0L < 0

let compare_exact a b =
  match (a.neg, b.neg) with
  | true, false -> -1
  | false, true -> 1
  | false, false -> Int64.unsigned_compare a.magnitude b.magnitude
  | true, true -> Int64.unsigned_compare b.magnitude a.magnitude

let add_magnitude a b =
  let sum = Int64.add a b in
  if Int64.unsigned_compare sum a < 0 then raise Overflow else sum

let mul_magnitude a b =
  if a = 0L then 0L
  else
    let product = Int64.mul a b in
    if Int64.unsigned_div product a <> b then raise Overflow else product

let add a b =
  if a.neg = b.neg then
    of_magnitude ~neg:a.neg (add_magnitude a.magnitude b.magnitude)
  else if Int64.unsigned_compare a.magnitude b.magnitude >= 0 then
    of_magnitude ~neg:a.neg (Int64.sub a.magnitude b.magnitude)
  else of_magnitude ~neg:b.neg (Int64.sub b.magnitude a.magnitude)

let sub a b = add a (negate b)

let mul a b =
  of_magnitude ~neg:(a.neg <> b.neg) (mul_magnitude a.magnitude b.magnitude)

let div a b =
  of_magnitude ~neg:(a.neg <> b.neg)
    (Int64.unsigned_div a.magnitude b.magnitude)

let rem a b =
  of_magnitude ~neg:a.neg (Int64.unsigned_rem a.magnitude b.magnitude)

(* Two non negatives are already the bit pattern so nothing reads signed *)
let bitwise f a b =
  if (not a.neg) && not b.neg then of_magnitude (f a.magnitude b.magnitude)
  else if is_big a || is_big b then raise Overflow
  else of_bits (f (bits_of a) (bits_of b))

let lognot e = negate (add e one)

let shift_left a count =
  if Int64.unsigned_compare count 63L > 0 then
    if a.magnitude = 0L then zero else raise Overflow
  else
    let n = Int64.to_int count in
    let shifted = Int64.shift_left a.magnitude n in
    if Int64.shift_right_logical shifted n <> a.magnitude then raise Overflow
    else of_magnitude ~neg:a.neg shifted

(* A neg goes toward minus inf so its magnitude rounds up *)
let shift_right a count =
  if Int64.unsigned_compare count 63L > 0 then
    if a.neg then negate one else zero
  else
    let n = Int64.to_int count in
    if not a.neg then of_magnitude (Int64.shift_right_logical a.magnitude n)
    else
      let dropped = Int64.shift_right_logical (Int64.sub a.magnitude 1L) n in
      of_magnitude ~neg:true (Int64.add dropped 1L)

let representable kind e =
  let limit =
    if e.neg then int_kind_neg_limit kind else int_kind_pos_limit kind
  in
  Int64.unsigned_compare e.magnitude limit <= 0

(* A cast is where the source asked for this so it stays the only wrap *)
let narrow kind n =
  let width = int_kind_size kind * 8 in
  if width = 64 then
    if int_kind_unsigned kind then of_magnitude n else of_bits n
  else
    let masked = Int64.logand n (Int64.sub (Int64.shift_left 1L width) 1L) in
    if int_kind_unsigned kind then of_magnitude masked
    else
      let shift = 64 - width in
      of_bits (Int64.shift_right (Int64.shift_left masked shift) shift)

let int_of = function
  | VInt (e, _) -> bits_of e
  | VFloat (f, _) -> Int64.of_float f
  | VBool b -> if b then 1L else 0L
  | VChar c -> Int64.of_int c

let float_of = function
  | VFloat (f, _) -> f
  | (VInt _ | VBool _ | VChar _) as v -> Int64.to_float (int_of v)

let exact_of = function
  | VInt (e, _) -> e
  | VFloat (f, _) -> of_bits (Int64.of_float f)
  | VBool b -> if b then one else zero
  | VChar c -> of_magnitude (Int64.of_int c)

let of_float kind f =
  match kind with
  | F32 -> VFloat (Int32.float_of_bits (Int32.bits_of_float f), F32)
  | F64 -> VFloat (f, F64)

let of_int64 ty n =
  match resolve_ty ty with
  | TInt kind -> VInt (narrow kind n, kind)
  | TFloat kind -> of_float kind (Int64.to_float n)
  | TBool -> VBool (n <> 0L)
  | TChar -> VChar (Int64.to_int n)
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"
  | _ ->
      let kind = if is_wide_ty ty then I64 else I32 in
      VInt (narrow kind n, kind)

(* A literal already says what it is so nothing masks here *)
let of_literal ty n =
  match resolve_ty ty with
  | TInt kind ->
      let e = if int_kind_unsigned kind then of_magnitude n else of_bits n in
      VInt (e, kind)
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"
  | _ -> of_int64 ty n

(* An arithmetic result keeps its exact value so an overflow stays visible *)
let retype ty e =
  match resolve_ty ty with
  | TInt kind -> VInt (e, kind)
  | TFloat kind -> of_float kind (Int64.to_float (bits_of e))
  | TBool -> VBool (bits_of e <> 0L)
  | TChar -> VChar (Int64.to_int (bits_of e))
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"
  | _ -> VInt (e, if is_wide_ty ty then I64 else I32)

let unsupported_const span =
  Diagnostic.(
    error "unsupported constant expression"
    |> at span
    |> help "constant initializers must evaluate at compile time")

(* Only the arith lands here since a comp is not an int *)
let int_binop op a b =
  try
    match op with
    | Ast.Add -> Some (add a b)
    | Ast.Sub -> Some (sub a b)
    | Ast.Mul -> Some (mul a b)
    (* FIXME: This is temporary *)
    | Ast.Div when b.magnitude = 0L -> None
    | Ast.Div -> Some (div a b)
    | Ast.Mod when b.magnitude = 0L -> None
    | Ast.Mod -> Some (rem a b)
    | Ast.BitAnd -> Some (bitwise Int64.logand a b)
    | Ast.BitOr -> Some (bitwise Int64.logor a b)
    | Ast.BitXor -> Some (bitwise Int64.logxor a b)
    | Ast.Lshift -> Some (shift_left a (bits_of b))
    | Ast.Rshift -> Some (shift_right a (bits_of b))
    | _ -> None
  with Overflow -> None

let cast target v =
  match (resolve_ty target, v) with
  | TFloat kind, VFloat (f, _) -> of_float kind f
  | TFloat kind, _ -> of_float kind (float_of v)
  | _, VFloat (f, _) -> of_int64 target (Int64.of_float f)
  | _, _ -> of_int64 target (int_of v)

let unop op ~result_ty v =
  match (op, v) with
  | Ast.Pos, _ -> Some v
  | Ast.Neg, VFloat (f, kind) -> Some (of_float kind (-.f))
  | Ast.Neg, _ -> Some (retype result_ty (negate (exact_of v)))
  | Ast.BitNot, VFloat _ -> None
  | Ast.BitNot, _ -> (
      try Some (retype result_ty (lognot (exact_of v))) with Overflow -> None)
  | Ast.Not, VFloat _ -> None
  | Ast.Not, _ -> Some (VBool (int_of v = 0L))
  | (Ast.Deref | Ast.AddressOf), _ -> None

let binop op ~result_ty a b =
  match (a, b) with
  | VFloat _, _ | _, VFloat _ -> (
      let x = float_of a and y = float_of b in
      (* The kind is read in the arm so a compare never asks a bool for one *)
      (* FIXME(8d3d): A mismatch can leave an int here so it doesn't fold this works for now
         but ill look more into this *)
      let arith f =
        match resolve_ty result_ty with
        | TFloat kind -> Some (of_float kind (f x y))
        | _ -> None
      in
      let test b = Some (VBool b) in
      match op with
      | Ast.Add -> arith ( +. )
      | Ast.Sub -> arith ( -. )
      | Ast.Mul -> arith ( *. )
      | Ast.Div -> arith ( /. )
      | Ast.Eq -> test (x = y)
      | Ast.Neq -> test (x <> y)
      | Ast.Lt -> test (x < y)
      | Ast.Gt -> test (x > y)
      | Ast.Lte -> test (x <= y)
      | Ast.Gte -> test (x >= y)
      | _ -> None)
  | (VInt _ | VBool _ | VChar _), (VInt _ | VBool _ | VChar _) -> (
      let x = exact_of a and y = exact_of b in
      let test b = Some (VBool b) in
      let cmp = compare_exact x y in
      match (int_binop op x y, op) with
      | Some e, _ -> Some (retype result_ty e)
      | None, Ast.Eq -> test (cmp = 0)
      | None, Ast.Neq -> test (cmp <> 0)
      | None, Ast.Lt -> test (cmp < 0)
      | None, Ast.Gt -> test (cmp > 0)
      | None, Ast.Lte -> test (cmp <= 0)
      | None, Ast.Gte -> test (cmp >= 0)
      | None, Ast.And -> test (x.magnitude <> 0L && y.magnitude <> 0L)
      | None, Ast.Or -> test (x.magnitude <> 0L || y.magnitude <> 0L)
      | None, _ -> None)
