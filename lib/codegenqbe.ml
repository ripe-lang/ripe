(* SPDX-License-Identifier: Apache-2.0 *)

(* https://c9x.me/compile/doc/il.html *)
open Types

type qbe_scalar = B | H | W | L | S | D

let qbe_scalar t =
  match resolve_ty t with
  | TBool -> B
  | TChar | TEnum _ -> W
  | TInt k -> ( match int_kind_size k with 1 -> B | 2 -> H | 4 -> W | _ -> L)
  | TFloat F32 -> S
  | TFloat F64 -> D
  | TPointer _ | TPtr | TNull | TCStr | TFunc _ | TArray _ | TSlice _ | TStr ->
      L
  | TNever -> Diagnostic.ice "TNever has no QBE type"
  | TError -> Diagnostic.ice "TError has no QBE type"
  | TUnit -> Diagnostic.ice "TUnit has no QBE type"
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"

let scalar_letter = function
  | B -> "b"
  | H -> "h"
  | W -> "w"
  | L -> "l"
  | S -> "s"
  | D -> "d"

type qbe_base = W | L | S | D

(* QBE doesn't have sub word reg so a byte and a half word both live in a w *)
let qbe_base t : qbe_base =
  match qbe_scalar t with B | H | W -> W | L -> L | S -> S | D -> D

let qbe_ty t = match qbe_base t with W -> "w" | L -> "l" | S -> "s" | D -> "d"

(* The QBE mnemonic prefix, u for unsigned int types and pointers, s otherwise *)
let signedness t =
  match resolve_ty t with
  | TPointer _ | TPtr | TNull | TCStr | TChar | TBool -> "u"
  | t -> if is_unsigned t then "u" else "s"

(* This is the hard limit where libc call stops emitting one instruction per word *)
let bulk_mem_threshold = 64

(* s_ for single, d_ for double *)
let float_lit ty f =
  let prefix, digits =
    match resolve_ty ty with
    | TFloat F32 -> ("s_", 9)
    | TFloat F64 -> ("d_", 17)
    | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"
    | _ -> Diagnostic.ice "float literal requires a float type"
  in
  prefix ^ Printf.sprintf "%.*g" digits f

let alloc_instr structs t =
  match Layout.ty_align structs t with
  | 1 | 2 | 4 -> "alloc4"
  | 8 -> "alloc8"
  | 16 -> "alloc16"
  | a ->
      Diagnostic.ice
        (Printf.sprintf "no alloc instruction for %d byte alignment" a)

(* A narrow load says how to fill the rest of the register but a wide one can't *)
let qbe_load t =
  match qbe_scalar t with
  | (B | H | W) as s -> "load" ^ signedness t ^ scalar_letter s
  | (L | S | D) as s -> "load" ^ scalar_letter s

let qbe_store t = "store" ^ scalar_letter (qbe_scalar t)

type ctx = {
  structs : Layout.t;
  struct_names : string Symbol.Table.t;
  panics : Panictable.t;
  used_slots : (string, unit) Hashtbl.t;
  buf : Buffer.t;
  mutable strings : (string * string) list;
  abi_types : (ty, string) Hashtbl.t;
  abi_defs : Buffer.t;
  mutable next_abi_type : int;
  mutable tmp : int;
  tmp_names : string Dynarray.t;
  block_labels : string Dynarray.t;
  mutable str_ctr : int;
}

type local_binding = Value of string | Memory of string
type local_usage = { address_taken : bool array; defined : bool array }

type mir_ctx = {
  qbe : ctx;
  func : Mir.func;
  bindings : local_binding array;
  global_types : (string, ty) Hashtbl.t;
}

(* A function counts its temps and blocks from zero so we'd rather keep the names *)
let cached_name names spell n =
  while Dynarray.length names <= n do
    Dynarray.add_last names (spell (Dynarray.length names))
  done;
  Dynarray.get names n

(* A fresh temp gets a name like %t0 *)
let fresh ctx =
  let n = ctx.tmp in
  ctx.tmp <- n + 1;
  cached_name ctx.tmp_names (fun i -> "%t" ^ string_of_int i) n

let mir_block_label ctx id =
  cached_name ctx.block_labels
    (fun i -> if i = 0 then "@start" else "@block" ^ string_of_int i)
    id

(* This is to handle collisions because t0 or t1 would collide with the temps *)
let spelled_like_tmp name =
  String.length name > 1
  && name.[0] = 't'
  && String.for_all Char.Ascii.is_digit (String.drop_first 1 name)

let emit ctx fmt = Printf.bprintf ctx.buf fmt

(* These three shapes cover almost everything so they skip the format interpreter *)
let put ctx s = Buffer.add_string ctx.buf s
let put_char ctx c = Buffer.add_char ctx.buf c

let emit_op ctx dest ty op =
  put ctx dest;
  put ctx " =";
  put ctx ty;
  put_char ctx ' ';
  put ctx op;
  put_char ctx ' '

let emit_op1 ctx dest ty op arg =
  emit_op ctx dest ty op;
  put ctx arg;
  put_char ctx '\n'

let emit_op2 ctx dest ty op lhs rhs =
  emit_op ctx dest ty op;
  put ctx lhs;
  put ctx ", ";
  put ctx rhs;
  put_char ctx '\n'

let emit_store ctx store value addr =
  put ctx store;
  put_char ctx ' ';
  put ctx value;
  put ctx ", ";
  put ctx addr;
  put_char ctx '\n'

(* The data pointer sits at offset 0 in the fat pointer *)
let load_slice_ptr ctx addr =
  let ptr = fresh ctx in
  emit_op1 ctx ptr "l" "loadl" addr;
  ptr

(* The length sits at offset 8 in the fat pointer *)
let load_slice_len ctx addr =
  let lenp = fresh ctx in
  emit_op2 ctx lenp "l" "add" addr "8";
  let len = fresh ctx in
  emit_op1 ctx len "l" "loadl" lenp;
  len

let intern_string ctx s =
  let lbl = Printf.sprintf "$str.%d" ctx.str_ctr in
  ctx.str_ctr <- ctx.str_ctr + 1;
  ctx.strings <- (lbl, s) :: ctx.strings;
  lbl

let emit_label ctx lbl =
  put ctx lbl;
  put_char ctx '\n'

let emit_jmp ctx lbl =
  put ctx "jmp ";
  put ctx lbl;
  put_char ctx '\n'

let emit_jnz ctx v then_lbl else_lbl =
  put ctx "jnz ";
  put ctx v;
  put ctx ", ";
  put ctx then_lbl;
  put ctx ", ";
  put ctx else_lbl;
  put_char ctx '\n'

let offset_addr ctx base off =
  if off = 0 then base
  else
    let a = fresh ctx in
    emit_op2 ctx a "l" "add" base (string_of_int off);
    a

(* The pointer math is 64 bit so a word sized value widens to a long *)
let widen_to_l ctx v ty =
  if qbe_base ty = L then v
  else
    let t = fresh ctx in
    let ins = if is_unsigned ty then "extuw" else "extsw" in
    emit_op1 ctx t "l" ins v;
    t

let bounds_condition ctx idx len =
  let cond = fresh ctx in
  emit_op2 ctx cond "w" "cugel" idx len;
  cond

let slice_bounds_condition ctx lo hi len =
  let hi_bad = fresh ctx in
  emit ctx "%s =w cugtl %s, %s\n" hi_bad hi len;
  let lo_bad = fresh ctx in
  emit ctx "%s =w cugtl %s, %s\n" lo_bad lo hi;
  let bad = fresh ctx in
  emit ctx "%s =w or %s, %s\n" bad hi_bad lo_bad;
  bad

let null_condition ctx ptr =
  let isnull = fresh ctx in
  emit ctx "%s =w ceql %s, 0\n" isnull ptr;
  isnull

let div_zero_condition ctx divisor op_qt =
  let zero = fresh ctx in
  emit ctx "%s =w ceq%s %s, 0\n" zero op_qt divisor;
  zero

let negative_shift_condition ctx count count_qt =
  let neg = fresh ctx in
  emit ctx "%s =w cslt%s %s, 0\n" neg count_qt count;
  neg

(* This writes a zero value of type t to dest *)
let emit_zero_into ctx dest t =
  if is_aggregate t then begin
    let size = Layout.ty_size ctx.structs t in
    if size > bulk_mem_threshold then
      emit ctx "call $memset(l %s, w 0, l %d)\n" dest size
    else begin
      let align = Layout.ty_align ctx.structs t in
      let off = ref 0 in
      let step w store =
        while w <= align && !off + w <= size do
          emit_store ctx store "0" (offset_addr ctx dest !off);
          off := !off + w
        done
      in
      step 8 "storel";
      step 4 "storew";
      step 2 "storeh";
      step 1 "storeb"
    end
  end
  else emit_store ctx (qbe_store t) "0" dest

(* An aggregate suhc as a slice is moved by copying size bytes from src to dest *)
let emit_aggregate_copy ctx dest src size =
  if size > bulk_mem_threshold then
    emit ctx "call $memcpy(l %s, l %s, l %d)\n" dest src size
  else begin
    put ctx "blit ";
    put ctx src;
    put ctx ", ";
    put ctx dest;
    put ctx ", ";
    put ctx (string_of_int size);
    put_char ctx '\n'
  end

let rec emit_arith_binop ctx op ~result_ty:t ~operand_ty:lty ~count_ty lv rv =
  let qt = qbe_ty t in
  let op_qt = qbe_ty lty in
  let sign = signedness lty in
  let unsigned = is_unsigned lty in

  let tmp = fresh ctx in
  (* The float ones are clts and cltd with no sign prefix and the int ones are csltw, csltl, cultw and so on *)
  let compare name =
    let prefix = if is_float lty then "c" else "c" ^ sign in
    emit_op2 ctx tmp "w" (prefix ^ name ^ op_qt) lv rv
  in

  (match op with
  | Mir.Add -> emit_op2 ctx tmp qt "add" lv rv
  | Mir.Sub -> emit_op2 ctx tmp qt "sub" lv rv
  | Mir.Mul -> emit_op2 ctx tmp qt "mul" lv rv
  | Mir.Div -> emit_op2 ctx tmp qt (if unsigned then "udiv" else "div") lv rv
  | Mir.Mod -> emit_op2 ctx tmp qt (if unsigned then "urem" else "rem") lv rv
  (* The float ones are ceqs and ceqd and the int ones are ceqw and ceql *)
  | Mir.Eq -> emit_op2 ctx tmp "w" ("ceq" ^ op_qt) lv rv
  | Mir.Neq -> emit_op2 ctx tmp "w" ("cne" ^ op_qt) lv rv
  | Mir.Lt -> compare "lt"
  | Mir.Gt -> compare "gt"
  | Mir.Lte -> compare "le"
  | Mir.Gte -> compare "ge"
  | Mir.BitAnd -> emit_op2 ctx tmp qt "and" lv rv
  | Mir.BitOr -> emit_op2 ctx tmp qt "or" lv rv
  | Mir.BitXor -> emit_op2 ctx tmp qt "xor" lv rv
  | Mir.Lshift -> emit_op2 ctx tmp qt "shl" lv (word_count ctx count_ty rv)
  | Mir.Rshift ->
      emit_op2 ctx tmp qt
        (if unsigned then "shr" else "sar")
        lv
        (word_count ctx count_ty rv));
  match op with
  | Mir.Add | Mir.Sub | Mir.Mul | Mir.Div | Mir.Lshift ->
      narrow_int_to ctx tmp t
  | _ -> tmp

(* The shift wants a word count so a long one drops to its low word *)
and word_count ctx count_ty rv =
  match qbe_base count_ty with
  | L ->
      let w = fresh ctx in
      emit_op1 ctx w "w" "copy" rv;
      w
  | _ -> rv

(* The extend that masks a narrow integer target back down to its width *)
and narrow_int_instr t =
  match resolve_ty t with
  | TInt I8 -> Some "extsb"
  | TInt U8 -> Some "extub"
  | TInt I16 -> Some "extsh"
  | TInt U16 -> Some "extuh"
  | _ -> None

(* A narrow int only fills the low bits of its w register so a cast has to mask it back down *)
and narrow_int_to ctx v target_ty =
  match narrow_int_instr target_ty with
  | None -> v
  | Some instr ->
      let tmp = fresh ctx in
      emit_op1 ctx tmp "w" instr v;
      tmp

and emit_cast ctx v src_ty target_ty =
  let tmp = fresh ctx in
  let tgt = qbe_ty target_ty in
  let src_base = qbe_base src_ty in
  (* The extend already truncates so the copy would be redundant *)
  match (narrow_int_instr target_ty, src_base) with
  | Some instr, (W | L) ->
      emit_op1 ctx tmp "w" instr v;
      tmp
  | _ ->
      let src_unsigned = is_unsigned src_ty in
      let target_unsigned = is_unsigned target_ty in
      let ty, instr =
        match (src_base, qbe_base target_ty) with
        | W, W | L, L | S, S | D, D -> (tgt, "copy")
        | W, L -> ("l", if src_unsigned then "extuw" else "extsw")
        | L, W -> ("w", "copy")
        | S, D -> ("d", "exts")
        | D, S -> ("s", "truncd")
        | W, (S | D) -> (tgt, if src_unsigned then "uwtof" else "swtof")
        | L, (S | D) -> (tgt, if src_unsigned then "ultof" else "sltof")
        | S, (W | L) -> (tgt, if target_unsigned then "stoui" else "stosi")
        | D, (W | L) -> (tgt, if target_unsigned then "dtoui" else "dtosi")
      in
      emit_op1 ctx tmp ty instr v;
      narrow_int_to ctx tmp target_ty

let qbe_struct_name ctx name =
  Symbol.Table.find ctx.struct_names (Keyname.key name)

let rec qbe_ext_ty (struct_name : Keyname.t -> string) t =
  match resolve_ty t with
  | TStruct (sn, _) -> ":" ^ struct_name sn
  (* QBE repeats a field type so { w 3 } means three words *)
  | None, TArray (e, n) ->
      (* A QBE field cannot nest counts, so every dimension collapses into one *)
      let rec flatten t reps =
        match resolve_ty t with
        | TArray (e, n) -> flatten e (reps * n)
        | TSlice _ -> ("l", reps * 2)
        | _ -> (qbe_ext_ty ctx t, reps)
      in
      let unit_ty, reps = flatten e n in
      Printf.sprintf "%s %d" unit_ty reps
  | TSlice _ | TStr -> "l 2"
  | _ -> scalar_letter (qbe_scalar t)

(* A field that takes up no bytes changes nothing here, and leaving one in makes QBE think the whole struct is empty *)
let emit_struct_type ctx name fields =
  let struct_name = qbe_struct_name ctx in
  let keep ft rest =
    if Layout.ty_size ctx.structs ft > 0 then qbe_ext_ty struct_name ft :: rest
    else rest
  in
  let field_strs = Iarray.fold_right keep fields [] in
  emit ctx "type :%s = { %s }\n" (qbe_struct_name ctx name)
    (String.concat ", " field_strs)

let escape_data_string content =
  let buf = Buffer.create (String.length content) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | ('\000' .. '\031' | '\127') as c ->
          Buffer.add_string buf (Printf.sprintf "\\%03o" (Char.code c))
      | c -> Buffer.add_char buf c)
    content;
  Buffer.contents buf

let emit_string_data ctx lbl content =
  emit ctx "data %s = { b \"%s\", b 0 }\n" lbl (escape_data_string content)

let emit_string_into ctx destination content =
  let label = intern_string ctx content in
  emit ctx "storel %s, %s\n" label destination;
  emit ctx "storel %d, %s\n" (String.length content)
    (offset_addr ctx destination 8)

let direct_value_binding mctx place =
  let value_name = function Value name -> Some name | Memory _ -> None in
  match (place.Mir.place_base, place.Mir.place_projs) with
  | Mir.Local id, [] -> value_name mctx.bindings.(id)
  | Mir.Local _, _ | Mir.Global _, _ -> None

let mir_base_ty mctx base =
  match base with
  | Mir.Local id -> mctx.func.Mir.func_locals.(id).Mir.local_ty
  | Mir.Global name -> Hashtbl.find mctx.global_types name

let rec emit_mir_operand mctx operand =
  match operand.Mir.op_desc with
  | Mir.Const constant -> emit_mir_constant mctx.qbe operand.Mir.op_ty constant
  | Mir.Copy place -> emit_mir_copy mctx place

and emit_mir_copy mctx place =
  match direct_value_binding mctx place with
  | Some value -> value
  | None ->
      let addr, ty = emit_mir_place mctx place in
      if is_aggregate ty then addr
      else
        let value = fresh mctx.qbe in
        emit_op1 mctx.qbe value (qbe_ty ty) (qbe_load ty) addr;
        value

and emit_mir_constant ctx ty = function
  | Mir.Int value -> Int64.to_string value
  | Mir.Float value -> float_lit ty value
  | Mir.Bool value -> if value then "1" else "0"
  | Mir.Null | Mir.Zero | Mir.Undef -> "0"
  | Mir.CStr value -> intern_string ctx value
  | Mir.Char value -> string_of_int value
  | Mir.Function name -> "$" ^ name
  | Mir.Str _ -> Diagnostic.ice "str constant requires a destination"

and widened_operand mctx operand =
  match operand.Mir.op_desc with
  | Mir.Const (Mir.Int value) -> Int64.to_string value
  | Mir.Const (Mir.Bool value) -> if value then "1" else "0"
  | Mir.Const (Mir.Char value) -> string_of_int value
  | _ -> widen_to_l mctx.qbe (emit_mir_operand mctx operand) operand.Mir.op_ty

and emit_mir_index_addr mctx storage elem index_operand =
  let ctx = mctx.qbe in
  let elem_stride = Layout.stride ctx.structs elem in
  match index_operand.Mir.op_desc with
  | Mir.Const (Mir.Int value) ->
      let offset = Int64.mul value (Int64.of_int elem_stride) in
      if offset = 0L then storage
      else begin
        let addr = fresh ctx in
        emit_op2 ctx addr "l" "add" storage (Int64.to_string offset);
        addr
      end
  | _ ->
      let index = widened_operand mctx index_operand in
      let offset = fresh ctx in
      emit_op2 ctx offset "l" "mul" index (string_of_int elem_stride);
      let addr = fresh ctx in
      emit_op2 ctx addr "l" "add" storage offset;
      addr

and emit_mir_place mctx place =
  let ctx = mctx.qbe in
  let rec project addr ty = function
    | [] -> (addr, ty)
    | Mir.Deref :: rest ->
        let ptr = fresh ctx in
        emit_op1 ctx ptr "l" "loadl" addr;
        let inner =
          match resolve_ty ty with
          | TPointer inner -> inner
          | _ -> Diagnostic.ice "MIR deref on non pointer"
        in
        project ptr inner rest
    | Mir.Field field :: rest ->
        let struct_name =
          match resolve_ty ty with
          | TStruct (name, _) -> name
          | _ -> Diagnostic.ice "MIR field on non struct"
        in
        let field_ty = Layout.struct_field_ty ctx.structs struct_name field in
        let addr =
          offset_addr ctx addr
            (Layout.field_offset ctx.structs struct_name field)
        in
        project addr field_ty rest
    | Mir.Index index_operand :: rest ->
        let elem, storage =
          match resolve_ty ty with
          | TArray (elem, _) -> (elem, addr)
          | TSlice elem -> (elem, load_slice_ptr ctx addr)
          | TPointer elem ->
              let ptr = fresh ctx in
              emit_op1 ctx ptr "l" "loadl" addr;
              (elem, ptr)
          | _ -> Diagnostic.ice "MIR index on non indexed place"
        in
        let addr = emit_mir_index_addr mctx storage elem index_operand in
        project addr elem rest
  in
  let base_ty = mir_base_ty mctx place.Mir.place_base in
  let projections = List.rev place.Mir.place_projs in
  let project_ptr ptr projection rest =
    match (resolve_ty base_ty, projection) with
    | TPointer elem, Mir.Deref -> project ptr elem rest
    | TPointer elem, Mir.Index index ->
        let addr = emit_mir_index_addr mctx ptr elem index in
        project addr elem rest
    | _, _ -> Diagnostic.ice "MIR pointer projection on non pointer"
  in
  match place.Mir.place_base with
  | Mir.Global name -> project ("$" ^ name) base_ty projections
  | Mir.Local id ->
      begin match (mctx.bindings.(id), projections) with
      | Memory addr, projections -> project addr base_ty projections
      | Value ptr, Mir.Deref :: rest -> project_ptr ptr Mir.Deref rest
      | Value ptr, Mir.Index index :: rest ->
          project_ptr ptr (Mir.Index index) rest
      | Value _, [] -> Diagnostic.ice "value local used as an address"
      | Value _, Mir.Field _ :: _ ->
          Diagnostic.ice "MIR projection on scalar local"
      end

let emit_mir_unop ctx op operand ty =
  let qt = qbe_ty ty in
  let result = fresh ctx in
  match op with
  | Mir.Neg ->
      emit_op1 ctx result qt "neg" operand;
      narrow_int_to ctx result ty
  | Mir.BitNot ->
      emit_op2 ctx result qt "xor" operand "-1";
      narrow_int_to ctx result ty
  | Mir.Not ->
      emit_op2 ctx result "w" "ceqw" operand "0";
      result

let emit_mir_value mctx value =
  let ctx = mctx.qbe in
  match value.Mir.val_desc with
  | Mir.Use operand -> emit_mir_operand mctx operand
  | Mir.UnOp (op, operand) ->
      emit_mir_unop ctx op (emit_mir_operand mctx operand) value.Mir.val_ty
  | Mir.BinOp (op, left, right) ->
      let left_value = emit_mir_operand mctx left in
      let right_value = emit_mir_operand mctx right in
      emit_arith_binop ctx op ~result_ty:value.Mir.val_ty
        ~operand_ty:left.Mir.op_ty ~count_ty:right.Mir.op_ty left_value
        right_value
  | Mir.Cast operand ->
      emit_cast ctx
        (emit_mir_operand mctx operand)
        operand.Mir.op_ty value.Mir.val_ty
  | Mir.AddressOf place -> fst (emit_mir_place mctx place)
  | Mir.Len place -> (
      let addr, ty = emit_mir_place mctx place in
      match resolve_ty ty with
      | TArray (_, count) -> string_of_int count
      | TSlice _ | TStr -> load_slice_len ctx addr
      | _ -> Diagnostic.ice "MIR len on invalid place")
  | Mir.DataPtr place -> (
      let addr, ty = emit_mir_place mctx place in
      match resolve_ty ty with
      | TArray _ -> addr
      | TSlice _ | TStr -> load_slice_ptr ctx addr
      | _ -> Diagnostic.ice "MIR data pointer on invalid place")
  | Mir.SizeOf ty -> string_of_int (Layout.ty_size ctx.structs ty)

let emit_value_into mctx destination destination_ty value =
  match direct_value_binding mctx destination with
  | Some binding ->
      emit_op1 mctx.qbe binding (qbe_ty destination_ty) "copy" value
  | None ->
      let destination_addr, destination_ty = emit_mir_place mctx destination in
      if is_aggregate destination_ty then
        emit_aggregate_copy mctx.qbe destination_addr value
          (Layout.ty_size mctx.qbe.structs destination_ty)
      else emit_store mctx.qbe (qbe_store destination_ty) value destination_addr

let emit_mir_assign mctx destination value =
  let ctx = mctx.qbe in
  match value.Mir.val_desc with
  | Mir.Use { Mir.op_desc = Mir.Const Mir.Undef; _ } -> ()
  | Mir.Use { Mir.op_desc = Mir.Const (Mir.Str content); _ } ->
      let destination_addr, _ = emit_mir_place mctx destination in
      emit_string_into ctx destination_addr content
  | Mir.Use { Mir.op_desc = Mir.Const Mir.Zero; _ } ->
      let destination_ty = value.Mir.val_ty in
      if is_aggregate destination_ty then begin
        let destination_addr, _ = emit_mir_place mctx destination in
        emit_zero_into ctx destination_addr destination_ty
      end
      else emit_value_into mctx destination destination_ty "0"
  | _ ->
      let result = emit_mir_value mctx value in
      emit_value_into mctx destination value.Mir.val_ty result

let emit_mir_slice mctx destination source lo hi =
  let ctx = mctx.qbe in
  let destination_addr, destination_ty = emit_mir_place mctx destination in
  let source_addr, source_ty = emit_mir_place mctx source in
  let storage =
    match resolve_ty source_ty with
    | TArray _ -> source_addr
    | TSlice _ -> load_slice_ptr ctx source_addr
    | _ -> Diagnostic.ice "MIR slice requires array or slice"
  in
  let elem =
    match resolve_ty destination_ty with
    | TSlice elem -> elem
    | _ -> Diagnostic.ice "MIR slice result has invalid type"
  in
  let lo_value = widened_operand mctx lo in
  let hi_value = widened_operand mctx hi in
  let ptr = emit_mir_index_addr mctx storage elem lo in
  let len = fresh ctx in
  emit ctx "%s =l sub %s, %s\n" len hi_value lo_value;
  emit ctx "storel %s, %s\n" ptr destination_addr;
  emit ctx "storel %s, %s\n" len (offset_addr ctx destination_addr 8)

(* The condition is true when the check fails so it feeds the branch to the panic block *)
let emit_mir_check_condition mctx = function
  | Mir.Bounds (index, len) ->
      bounds_condition mctx.qbe
        (widened_operand mctx index)
        (widened_operand mctx len)
  | Mir.SliceBounds (lo, hi, len) ->
      slice_bounds_condition mctx.qbe (widened_operand mctx lo)
        (widened_operand mctx hi) (widened_operand mctx len)
  | Mir.Null ptr -> null_condition mctx.qbe (emit_mir_operand mctx ptr)
  | Mir.DivZero divisor ->
      div_zero_condition mctx.qbe
        (emit_mir_operand mctx divisor)
        (qbe_ty divisor.Mir.op_ty)
  | Mir.NegativeShift count ->
      negative_shift_condition mctx.qbe
        (emit_mir_operand mctx count)
        (qbe_ty count.Mir.op_ty)

let emit_mir_panic mctx span check =
  let ctx = mctx.qbe in
  let site = Panictable.record ctx.panics span in
  (match check with
  | Mir.Bounds (index, len) ->
      emit ctx "call $ripe_panic_bounds(w %d, l %s, l %s)\n" site
        (widened_operand mctx index)
        (widened_operand mctx len)
  | Mir.SliceBounds (lo, hi, len) ->
      emit ctx "call $ripe_panic_slice_bounds(w %d, l %s, l %s, l %s)\n" site
        (widened_operand mctx lo) (widened_operand mctx hi)
        (widened_operand mctx len)
  | Mir.Null _ -> emit ctx "call $ripe_panic_null(w %d)\n" site
  | Mir.DivZero _ -> emit ctx "call $ripe_panic_divzero(w %d)\n" site
  | Mir.NegativeShift _ -> emit ctx "call $ripe_panic_shift(w %d)\n" site);
  emit ctx "hlt\n"

let mir_callee mctx = function
  | Mir.Direct name -> "$" ^ name
  | Mir.Indirect operand -> emit_mir_operand mctx operand

let qbe_aggregate_abi_ty ctx aggregate =
  match Hashtbl.find_opt ctx.abi_types aggregate with
  | Some name -> ":" ^ name
  | None ->
      let id = ctx.next_abi_type in
      ctx.next_abi_type <- id + 1;
      let name = Printf.sprintf "RipeAbi%d" id in
      Hashtbl.add ctx.abi_types aggregate name;
      Printf.bprintf ctx.abi_defs "type :%s = { %s }\n" name
        (qbe_ext_ty ctx aggregate);
      ":" ^ name

let qbe_abi_ty ctx ty =
  match resolve_ty ty with
  | TStruct (name, _) -> ":" ^ qbe_struct_name ctx name
  | (TArray _ | TSlice _ | TStr) as aggregate ->
      qbe_aggregate_abi_ty ctx aggregate
  | _ -> qbe_ty ty

let qbe_call_ty ctx kind ty =
  match kind with
  | Mir.External when is_aggregate ty -> qbe_abi_ty ctx ty
  | _ -> qbe_ty ty

(* C hands an aggregate over by value so QBE has to see the layout *)
let qbe_param_ty ctx abi ty =
  if abi = C && is_aggregate ty then qbe_abi_ty ctx ty else qbe_ty ty

let c_result_slot ctx func bindings =
  match func.Mir.func_result with
  | Some id when func.Mir.func_abi = C && is_aggregate func.Mir.func_ret -> (
      match bindings.(id) with Value name | Memory name -> Some name)
  | _ -> None

let emit_mir_call mctx call =
  let ctx = mctx.qbe in
  let hidden_result =
    match (call.Mir.call_dest, call.Mir.call_ret, call.Mir.call_kind) with
    | Some destination, ret_ty, Mir.Internal when is_aggregate ret_ty ->
        Some (emit_mir_place mctx destination |> fst)
    | _ -> None
  in
  let args =
    List.map
      (fun operand ->
        Printf.sprintf "%s %s"
          (qbe_call_ty ctx call.Mir.call_kind operand.Mir.op_ty)
          (emit_mir_operand mctx operand))
      call.Mir.call_args
  in
  let args =
    match hidden_result with
    | Some destination -> Printf.sprintf "l %s" destination :: args
    | None -> args
  in
  let args =
    match call.Mir.call_variadic_start with
    | None -> args
    | Some count -> List.take count args @ ("..." :: List.drop count args)
  in
  let callee = mir_callee mctx call.Mir.call_callee in
  match (call.Mir.call_dest, call.Mir.call_ret, call.Mir.call_kind) with
  | None, _, _ -> emit ctx "call %s(%s)\n" callee (String.concat ", " args)
  | Some _, ret_ty, Mir.Internal when is_aggregate ret_ty ->
      emit ctx "call %s(%s)\n" callee (String.concat ", " args)
  | Some destination, ret_ty, _ ->
      let result = fresh ctx in
      emit ctx "%s =%s call %s(%s)\n" result
        (qbe_call_ty ctx call.Mir.call_kind ret_ty)
        callee (String.concat ", " args);
      emit_value_into mctx destination ret_ty result

let emit_mir_stmt mctx stmt =
  match stmt.Mir.stmt_desc with
  | Mir.Assign (destination, value) -> emit_mir_assign mctx destination value
  | Mir.Call call -> emit_mir_call mctx call
  | Mir.Slice (destination, source, lo, hi) ->
      emit_mir_slice mctx destination source lo hi

let emit_mir_terminator mctx terminator =
  let ctx = mctx.qbe in
  match terminator.Mir.term_desc with
  | Mir.Jump target -> emit_jmp ctx (mir_block_label ctx target)
  | Mir.Branch (condition, yes, no) ->
      emit_jnz ctx
        (emit_mir_operand mctx condition)
        (mir_block_label ctx yes) (mir_block_label ctx no)
  | Mir.Check (check, ok) ->
      let fail = "@panic" ^ string_of_int ok in
      emit_jnz ctx
        (emit_mir_check_condition mctx check)
        fail (mir_block_label ctx ok);
      emit_label ctx fail;
      emit_mir_panic mctx terminator.Mir.term_span check
  | Mir.ReturnValue None -> (
      match c_result_slot ctx mctx.func mctx.bindings with
      | Some slot -> emit ctx "ret %s\n" slot
      | None -> put ctx "ret\n")
  | Mir.ReturnValue (Some returned) ->
      let returned = emit_mir_operand mctx returned in
      put ctx "ret ";
      put ctx returned;
      put_char ctx '\n'
  | Mir.Unreachable -> put ctx "hlt\n"

let analyze_local_usage func =
  let address_taken = Array.make (Array.length func.Mir.func_locals) false in
  let defined = Array.make (Array.length func.Mir.func_locals) false in
  let mark_defined place =
    match (place.Mir.place_base, place.Mir.place_projs) with
    | Mir.Local id, [] -> defined.(id) <- true
    | Mir.Local _, _ | Mir.Global _, _ -> ()
  in
  let mark_stmt stmt =
    match stmt.Mir.stmt_desc with
    | Mir.Assign (destination, value) ->
        begin match value.Mir.val_desc with
        | Mir.Use { Mir.op_desc = Mir.Const Mir.Undef; _ } -> ()
        | Mir.AddressOf { Mir.place_base = Mir.Local id; place_projs = []; _ }
          ->
            mark_defined destination;
            address_taken.(id) <- true
        | _ -> mark_defined destination
        end
    | Mir.Call call -> Option.iter mark_defined call.Mir.call_dest
    | Mir.Slice (destination, _, _, _) -> mark_defined destination
  in
  let mark_block block = List.iter mark_stmt block.Mir.block_stmts in
  Array.iter mark_block func.Mir.func_blocks;
  List.iter (fun id -> defined.(id) <- true) func.Mir.func_params;
  { address_taken; defined }

let can_bind_value local =
  match resolve_ty local.Mir.local_ty with
  | TInt _ | TFloat _ | TBool | TChar | TEnum _ -> true
  | TPointer _ | TPtr | TNull | TCStr | TFunc _ -> true
  | _ -> false

(* This keeps names readable in the generated IL and adds suffixes when needed *)
let bind_mir_locals ctx func =
  let usage = analyze_local_usage func in
  Array.mapi
    (fun id local ->
      let base =
        match local.Mir.local_name with
        | Some n -> n
        | None -> "m" ^ string_of_int id
      in
      let slot =
        if Hashtbl.mem ctx.used_slots base || spelled_like_tmp base then
          base ^ "." ^ string_of_int id
        else base
      in
      Hashtbl.add ctx.used_slots slot ();
      let name = "%" ^ slot in
      if
        can_bind_value local && usage.defined.(id)
        && not usage.address_taken.(id)
      then Value name
      else Memory name)
    func.Mir.func_locals

let emit_mir_func ctx global_types func =
  Panictable.enter_func ctx.panics func.Mir.func_source_name;
  ctx.tmp <- 0;
  Hashtbl.clear ctx.used_slots;
  let bindings = bind_mir_locals ctx func in
  let returns_itself = c_result_slot ctx func bindings <> None in
  (* The result local lives in the caller's storage so it borrows the hidden pointer *)
  let result_tmp =
    match func.Mir.func_result with
    | Some id when not returns_itself ->
        let tmp = fresh ctx in
        bindings.(id) <- Memory tmp;
        Some tmp
    | _ -> None
  in
  let params =
    List.map
      (fun id ->
        let local = func.Mir.func_locals.(id) in
        let tmp = fresh ctx in
        (id, local.Mir.local_ty, tmp))
      func.Mir.func_params
  in
  let params_text =
    (match result_tmp with None -> [] | Some tmp -> [ "l " ^ tmp ])
    @ List.map
        (fun (_, ty, tmp) -> qbe_param_ty ctx func.Mir.func_abi ty ^ " " ^ tmp)
        params
  in
  let return_text =
    if
      is_unit func.Mir.func_ret || is_never func.Mir.func_ret
      || Option.is_some result_tmp
    then ""
    else qbe_abi_ty ctx func.Mir.func_ret ^ " "
  in
  emit ctx "%sfunction %s$%s(%s) {\n"
    (if func.Mir.func_export then "export " else "")
    return_text func.Mir.func_name
    (String.concat ", " params_text);
  emit_label ctx "@start";
  Array.iteri
    (fun id local ->
      match bindings.(id) with
      | Value _ -> ()
      | Memory slot ->
          if returns_itself || Some id <> func.Mir.func_result then
            emit_op1 ctx slot "l"
              (alloc_instr ctx.structs local.Mir.local_ty)
              (string_of_int (Layout.ty_size ctx.structs local.Mir.local_ty)))
    func.Mir.func_locals;
  List.iter
    (fun (id, ty, tmp) ->
      match bindings.(id) with
      | Value binding ->
          emit_op1 ctx binding (qbe_ty ty) "copy" (narrow_int_to ctx tmp ty)
      | Memory slot ->
          if is_aggregate ty then
            emit_aggregate_copy ctx slot tmp (Layout.ty_size ctx.structs ty)
          else emit_store ctx (qbe_store ty) tmp slot)
    params;
  let mctx = { qbe = ctx; func; bindings; global_types } in
  Array.iteri
    (fun id block ->
      if id > 0 then emit_label ctx (mir_block_label ctx id);
      List.iter (emit_mir_stmt mctx) block.Mir.block_stmts;
      match block.Mir.block_term with
      | Some terminator -> emit_mir_terminator mctx terminator
      | None -> Diagnostic.ice "unterminated verified MIR block")
    func.Mir.func_blocks;
  emit ctx "}\n\n"

(* The C runtime wants an int from main even if ripe's main gives nothing *)
let emit_entry_wrapper ctx func =
  emit ctx "export function w $main() {\n";
  emit_label ctx "@start";
  if resolve_ty func.Mir.func_ret = TInt I32 then begin
    let code = fresh ctx in
    emit ctx "%s =w call $%s()\n" code func.Mir.func_name;
    emit ctx "ret %s\n" code
  end
  else begin
    emit ctx "call $%s()\n" func.Mir.func_name;
    emit ctx "ret 0\n"
  end;
  emit ctx "}\n\n"

let rec emit_mir_global_fields ctx expected = function
  | Mir.GlobalConst (Mir.Zero, ty) ->
      Printf.sprintf "z %d" (Layout.ty_size ctx.structs ty)
  | Mir.GlobalConst (Mir.Undef, ty) ->
      Printf.sprintf "z %d" (Layout.ty_size ctx.structs ty)
  | Mir.GlobalConst (Mir.Str content, _) ->
      let label = intern_string ctx content in
      Printf.sprintf "l %s, l %d" label (String.length content)
  | Mir.GlobalConst (constant, ty) ->
      Printf.sprintf "%s %s" (qbe_ext_ty ctx ty)
        (emit_mir_constant ctx ty constant)
  | Mir.GlobalAddress name -> "l $" ^ name
  | Mir.GlobalArray values ->
      let elem =
        match resolve_ty expected with
        | TArray (elem, _) -> elem
        | _ -> Diagnostic.ice "MIR global array has invalid type"
      in
      String.concat ", " (List.map (emit_mir_global_fields ctx elem) values)
  | Mir.GlobalStruct values ->
      let name =
        match resolve_ty expected with
        | TStruct (name, _) -> name
        | _ -> Diagnostic.ice "MIR global struct has invalid type"
      in
      let offset = ref 0 in
      let parts = ref [] in
      let pad_to target =
        if target > !offset then
          parts := Printf.sprintf "z %d" (target - !offset) :: !parts
      in
      let field (field, value) =
        let field_ty = Layout.struct_field_ty ctx.structs name field in
        let at = Layout.field_offset ctx.structs name field in
        pad_to at;
        parts := emit_mir_global_fields ctx field_ty value :: !parts;
        offset := at + Layout.ty_size ctx.structs field_ty
      in
      List.iter field values;
      pad_to (Layout.ty_size ctx.structs expected);
      String.concat ", " (List.rev !parts)

let emit_mir_global ctx global =
  let align = Layout.ty_align ctx.structs global.Mir.global_ty in
  match global.Mir.global_init with
  | None ->
      emit ctx "data $%s = align %d { z %d }\n" global.Mir.global_name align
        (Layout.ty_size ctx.structs global.Mir.global_ty)
  | Some value ->
      emit ctx "data $%s = align %d { %s }\n" global.Mir.global_name align
        (emit_mir_global_fields ctx global.Mir.global_ty value)

let emit filename source_map program =
  let structs = Layout.create () in
  let struct_names = Symbol.Table.create 8 in
  let register name local =
    let key = Keyname.key name in
    Symbol.Table.add struct_names key
      (if local then Printf.sprintf "_Rlocal%d" (key :> int)
       else Keyname.show name)
  in
  List.iter
    (fun decl ->
      let key = Keyname.key decl.Mir.struct_name in
      Layout.set_struct_fields structs key decl.Mir.struct_fields;
      let name =
        if decl.Mir.struct_local then Printf.sprintf "_Rlocal%d" (key :> int)
        else Keyname.show decl.Mir.struct_name
      in
      Symbol.Table.add struct_names key name)
    program.Mir.prog_structs;
  let global_types =
    List.to_seq program.Mir.prog_globals
    |> Seq.map (fun g -> (g.Mir.global_name, g.Mir.global_ty))
    |> Hashtbl.of_seq
  in
  let ctx =
    {
      structs;
      struct_names;
      panics = Panictable.create filename source_map;
      used_slots = Hashtbl.create 16;
      buf = Buffer.create 1024;
      strings = [];
      abi_types = Hashtbl.create 8;
      abi_defs = Buffer.create 128;
      next_abi_type = 0;
      tmp = 0;
      tmp_names = Dynarray.create ();
      block_labels = Dynarray.create ();
      str_ctr = 0;
    }
  in
  (* The QBE format requires member structs before containing structs *)
  let emitted = Symbol.Table.create 8 in
  let rec emit_type name =
    let key = Keyname.key name in
    if not (Symbol.Table.mem emitted key) then begin
      Symbol.Table.add emitted key ();
      let fields = Layout.struct_fields ctx.structs key in
      Iarray.iter emit_dependencies fields;
      emit_struct_type ctx name fields
    end
  and emit_dependencies field_ty =
    match resolve_ty field_ty with
    | TStruct (name, _) -> emit_struct name
    | TArray (elem, _) -> emit_dependencies elem
    | _ -> ()
  in
  List.iter
    (fun decl -> emit_struct decl.Mir.struct_name)
    program.Mir.prog_structs;
  if not (List.is_empty program.Mir.prog_structs) then emit ctx "\n";
  let types_end = Buffer.length ctx.buf in
  List.iter (emit_mir_global ctx) program.Mir.prog_globals;
  if not (List.is_empty program.Mir.prog_globals) then emit ctx "\n";
  List.iter (emit_mir_func ctx global_types) program.Mir.prog_funcs;
  List.find_opt (fun func -> func.Mir.func_entry_point) program.Mir.prog_funcs
  |> Option.iter (emit_entry_wrapper ctx);
  (* A program with no checks emits neither table and the runtime declares both weak so it still links *)
  (match Panictable.sites ctx.panics with
  | [] -> ()
  | sites ->
      put ctx "export data $ripe_panic_strtab = { ";
      List.iteri
        (fun i s ->
          if i > 0 then put ctx ", ";
          put ctx "b \"";
          put ctx (escape_data_string s);
          put ctx "\", b 0")
        (Panictable.strings ctx.panics);
      put ctx " }\nexport data $ripe_panic_sites = align 4 { ";
      List.iteri
        (fun i site ->
          if i > 0 then put ctx ", ";
          emit ctx "w %d, w %d, w %d, w %d" site.Panictable.file site.line
            site.col site.func)
        sites;
      put ctx " }\n");
  List.iter
    (fun (label, content) -> emit_string_data ctx label content)
    (List.rev ctx.strings);
  let out = ctx.buf in
  if Buffer.length ctx.abi_defs = 0 then Buffer.contents out
  else
    Buffer.sub out 0 types_end
    ^ Buffer.contents ctx.abi_defs
    ^ Buffer.sub out types_end (Buffer.length out - types_end)
