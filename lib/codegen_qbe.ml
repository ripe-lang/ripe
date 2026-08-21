(* SPDX-License-Identifier: Apache-2.0 *)

(* Https://c9x.me/compile/doc/il.html *)
open Types

type qbe_scalar = B | H | W | L | S | D

let qbe_scalar (t : ty) =
  match resolve_ty t with
  | TBool -> B
  | TChar | TEnum _ -> W
  | TInt k -> ( match int_kind_size k with 1 -> B | 2 -> H | 4 -> W | _ -> L)
  | TFloat F32 -> S
  | TFloat F64 -> D
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ | TStruct _ | TArray _
  | TSlice _ | TStr ->
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
let qbe_base (t : ty) : qbe_base =
  match qbe_scalar t with B | H | W -> W | L -> L | S -> S | D -> D

let qbe_ty (t : ty) =
  match qbe_base t with W -> "w" | L -> "l" | S -> "s" | D -> "d"

(* The QBE mnemonic prefix, u for unsigned int types and pointers, s otherwise *)
let signedness (t : ty) =
  match resolve_ty t with
  | TPointer _ | TOpaquePtr | TNull | TCStr | TChar | TBool -> "u"
  | t -> if is_unsigned t then "u" else "s"

(* This is the hard limit where libc call stops emitting one instruction per word *)
let bulk_mem_threshold = 64

(* s_ for single, d_ for double *)
let float_lit (ty : ty) (f : float) =
  let prefix, digits =
    match resolve_ty ty with
    | TFloat F32 -> ("s_", 9)
    | TFloat F64 -> ("d_", 17)
    | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"
    | _ -> Diagnostic.ice "float literal requires a float type"
  in
  prefix ^ Printf.sprintf "%.*g" digits f

let alloc_instr (structs : ty list Symbol.Table.t) (t : ty) =
  match ty_align structs t with
  | 1 | 2 | 4 -> "alloc4"
  | 8 -> "alloc8"
  | 16 -> "alloc16"
  | a ->
      Diagnostic.ice
        (Printf.sprintf "no alloc instruction for %d byte alignment" a)

(* A narrow load says how to fill the rest of the register but a wide one can't *)
let qbe_load (t : ty) =
  match qbe_scalar t with
  | (B | H | W) as s -> "load" ^ signedness t ^ scalar_letter s
  | (L | S | D) as s -> "load" ^ scalar_letter s

let qbe_store (t : ty) = "store" ^ scalar_letter (qbe_scalar t)

type ctx = {
  structs : ty list Symbol.Table.t;
  struct_names : string Symbol.Table.t;
  panics : Panic_table.t;
  used_slots : (string, unit) Hashtbl.t;
  buf : Buffer.t ref;
  strings : (string * string) list ref;
  abi_types : (ty, string) Hashtbl.t;
  abi_defs : Buffer.t;
  next_abi_type : int ref;
  tmp : int ref;
  temp_names : string array ref;
  block_labels : string array ref;
  str_ctr : int ref;
}

type local_binding = Value of string | Memory of string

type local_usage = { address_taken : bool array; defined : bool array }

type mir_ctx = {
  qbe : ctx;
  func : Mir.func;
  bindings : local_binding array;
  global_types : (string, ty) Hashtbl.t;
}

(* Every function counts temps/blocks from 0 so I'll rather keep them *)
let cached_name cache spell n =
  let names = !cache in
  let have = Array.length names in
  if n < have then names.(n)
  else begin
    let want = max (n + 1) (2 * have) in
    let grown = Array.make want "" in
    Array.blit names 0 grown 0 have;
    for i = have to want - 1 do
      grown.(i) <- spell i
    done;
    cache := grown;
    grown.(n)
  end

(* Fresh temps use names like %t0 and %t1 *)
let fresh ctx =
  let n = !(ctx.tmp) in
  incr ctx.tmp;
  cached_name ctx.temp_names (fun i -> "%t" ^ string_of_int i) n

let mir_block_label ctx id =
  cached_name ctx.block_labels
    (fun i -> if i = 0 then "@start" else "@block" ^ string_of_int i)
    id

(* This is to handle collisions because t0 or t1 would collide with the temps *)
let spelled_like_temp name =
  String.length name > 1
  && name.[0] = 't'
  && String.for_all Char.Ascii.is_digit (String.drop_first 1 name)

let emit ctx fmt = Printf.bprintf !(ctx.buf) fmt

(* Three shapes basically cover everything so they skip the format interpreter *)
let put ctx s = Buffer.add_string !(ctx.buf) s

let put_char ctx c = Buffer.add_char !(ctx.buf) c

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
  let lbl = Printf.sprintf "$str.%d" !(ctx.str_ctr) in
  incr ctx.str_ctr;
  ctx.strings := (lbl, s) :: !(ctx.strings);
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

(* Pointer math is 64-bit so widen a word-sized value to a long *)
let widen_to_l ctx v ty =
  if qbe_base ty = L then v
  else
    let t = fresh ctx in
    let ins = if is_unsigned ty then "extuw" else "extsw" in
    emit_op1 ctx t "l" ins v;
    t

let bounds_condition (ctx : ctx) (idx : string) (len : string) =
  let cond = fresh ctx in
  emit_op2 ctx cond "w" "cugel" idx len;
  cond

let slice_bounds_condition (ctx : ctx) (lo : string) (hi : string)
    (len : string) =
  let hi_bad = fresh ctx in
  emit ctx "%s =w cugtl %s, %s\n" hi_bad hi len;
  let lo_bad = fresh ctx in
  emit ctx "%s =w cugtl %s, %s\n" lo_bad lo hi;
  let bad = fresh ctx in
  emit ctx "%s =w or %s, %s\n" bad hi_bad lo_bad;
  bad

let null_condition (ctx : ctx) (ptr : string) =
  let isnull = fresh ctx in
  emit ctx "%s =w ceql %s, 0\n" isnull ptr;
  isnull

let div_zero_condition (ctx : ctx) (divisor : string) (op_qt : string) =
  let zero = fresh ctx in
  emit ctx "%s =w ceq%s %s, 0\n" zero op_qt divisor;
  zero

let negative_shift_condition (ctx : ctx) (count : string) (count_qt : string) =
  let neg = fresh ctx in
  emit ctx "%s =w cslt%s %s, 0\n" neg count_qt count;
  neg

(* Write a zero value of type t to dest *)
let emit_zero_into ctx dest t =
  match resolve_ty t with
  | TArray _ | TSlice _ | TStruct _ ->
      let size = ty_size ctx.structs t in
      if size > bulk_mem_threshold then
        emit ctx "call $memset(l %s, w 0, l %d)\n" dest size
      else begin
        let align = ty_align ctx.structs t in
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
  | _ -> emit_store ctx (qbe_store t) "0" dest

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

let rec emit_arith_binop ctx (op : Mir.binop) ~result_ty:t ~operand_ty:lty
    ~count_ty lv rv =
  let qt = qbe_ty t in
  let op_qt = qbe_ty lty in
  let sign = signedness lty in
  let unsigned = is_unsigned lty in

  let tmp = fresh ctx in
  (* Floats: clts, cltd (no sign prefix) / ints: csltw, csltl, cultw, etc *)
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
  (* Floats: ceqs, ceqd / ints: ceqw, ceql *)
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
  (* The extend already truncates so the copy would be redundant *)
  match (narrow_int_instr target_ty, qbe_base src_ty) with
  | Some instr, (W | L) ->
      emit_op1 ctx tmp "w" instr v;
      tmp
  | _ ->
      (match (qbe_base src_ty, qbe_base target_ty) with
      | W, W | L, L | S, S | D, D -> emit_op1 ctx tmp tgt "copy" v
      | W, L ->
          let instr = if is_unsigned src_ty then "extuw" else "extsw" in
          emit_op1 ctx tmp "l" instr v
      | L, W -> emit_op1 ctx tmp "w" "copy" v
      | S, D -> emit ctx "%s =d exts %s\n" tmp v
      | D, S -> emit ctx "%s =s truncd %s\n" tmp v
      | (W | L), (S | D) ->
          let instr =
            match (qbe_base src_ty, is_unsigned src_ty) with
            | W, true -> "uwtof"
            | W, false -> "swtof"
            | L, true -> "ultof"
            | L, false -> "sltof"
            | (S | D), _ ->
                Diagnostic.ice "float to float conversion in integer path"
          in
          emit ctx "%s =%s %s %s\n" tmp tgt instr v
      | (S | D), (W | L) ->
          let instr =
            match (qbe_base src_ty, is_unsigned target_ty) with
            | S, true -> "stoui"
            | S, false -> "stosi"
            | D, true -> "dtoui"
            | D, false -> "dtosi"
            | (W | L), _ ->
                Diagnostic.ice "integer to integer conversion in float path"
          in
          emit ctx "%s =%s %s %s\n" tmp tgt instr v);
      narrow_int_to ctx tmp target_ty

let qbe_struct_name (ctx : ctx) (name : Qname.t) =
  Symbol.Table.find ctx.struct_names (Qname.key name)

let rec qbe_ext_ty (struct_name : Qname.t -> string) (t : ty) =
  match resolve_ty t with
  | TStruct (sn, _) -> ":" ^ struct_name sn
  (* QBE repeats a field type so { w 3 } means three words *)
  | TArray (e, n) ->
      (* A QBE field cannot nest counts, so every dimension collapses into one *)
      let rec flatten t reps =
        match resolve_ty t with
        | TArray (e, n) -> flatten e (reps * n)
        | TSlice _ -> ("l", reps * 2)
        | _ -> (qbe_ext_ty struct_name t, reps)
      in
      let unit_ty, reps = flatten e n in
      Printf.sprintf "%s %d" unit_ty reps
  | TSlice _ | TStr -> "l 2"
  | _ -> scalar_letter (qbe_scalar t)

(* A field that takes up no bytes changes nothing here, and leaving one in makes QBE think the whole struct is empty *)
let emit_struct_type (ctx : ctx) (name : Qname.t) (fields : ty list) =
  let fields = List.filter (fun ft -> ty_size ctx.structs ft > 0) fields in
  let field_strs = List.map (qbe_ext_ty (qbe_struct_name ctx)) fields in
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
      | c -> Buffer.add_char buf c)
    content;
  Buffer.contents buf

let emit_string_data (ctx : ctx) (lbl : string) (content : string) =
  emit ctx "data %s = { b \"%s\", b 0 }\n" lbl (escape_data_string content)

let emit_string_into ctx destination content =
  let label = intern_string ctx content in
  emit ctx "storel %s, %s\n" label destination;
  emit ctx "storel %d, %s\n" (String.length content)
    (offset_addr ctx destination 8)

let direct_value_binding (mctx : mir_ctx) (place : Mir.place) =
  match (place.Mir.base, place.Mir.projections) with
  | Mir.Local id, [] -> (
      match mctx.bindings.(id) with Value name -> Some name | Memory _ -> None)
  | Mir.Local _, _ | Mir.Global _, _ -> None

let mir_base_ty mctx (base : Mir.place_base) =
  match base with
  | Mir.Local id -> mctx.func.Mir.locals.(id).Mir.ty
  | Mir.Global name -> Hashtbl.find mctx.global_types name

let rec emit_mir_operand mctx (operand : Mir.operand) =
  match operand.Mir.desc with
  | Mir.Const constant -> emit_mir_constant mctx.qbe operand.Mir.ty constant
  | Mir.Copy place -> (
      match direct_value_binding mctx place with
      | Some value -> value
      | None ->
          let addr, ty = emit_mir_place mctx place in
          if is_aggregate ty then addr
          else
            let value = fresh mctx.qbe in
            emit_op1 mctx.qbe value (qbe_ty ty) (qbe_load ty) addr;
            value)

and emit_mir_constant ctx ty = function
  | Mir.Int value -> Int64.to_string value
  | Mir.Float value -> float_lit ty value
  | Mir.Bool value -> if value then "1" else "0"
  | Mir.Null | Mir.Zero | Mir.Undef -> "0"
  | Mir.CStr value -> intern_string ctx value
  | Mir.Char value -> string_of_int value
  | Mir.Function name -> "$" ^ name
  | Mir.Str _ -> Diagnostic.ice "str constant requires a destination"

and widened_operand (mctx : mir_ctx) (operand : Mir.operand) =
  match operand.Mir.desc with
  | Mir.Const (Mir.Int value) -> Int64.to_string value
  | Mir.Const (Mir.Bool value) -> if value then "1" else "0"
  | Mir.Const (Mir.Char value) -> string_of_int value
  | _ -> widen_to_l mctx.qbe (emit_mir_operand mctx operand) operand.Mir.ty

and emit_mir_index_addr (mctx : mir_ctx) storage element
    (index_operand : Mir.operand) =
  let ctx = mctx.qbe in
  let element_stride = stride ctx.structs element in
  match index_operand.Mir.desc with
  | Mir.Const (Mir.Int value) ->
      let offset = Int64.mul value (Int64.of_int element_stride) in
      if offset = 0L then storage
      else begin
        let addr = fresh ctx in
        emit_op2 ctx addr "l" "add" storage (Int64.to_string offset);
        addr
      end
  | _ ->
      let index = widened_operand mctx index_operand in
      let offset = fresh ctx in
      emit_op2 ctx offset "l" "mul" index (string_of_int element_stride);
      let addr = fresh ctx in
      emit_op2 ctx addr "l" "add" storage offset;
      addr

and emit_mir_place mctx (place : Mir.place) =
  let ctx = mctx.qbe in
  let rec project addr ty = function
    | [] -> (addr, ty)
    | Mir.Deref :: rest ->
        let pointer = fresh ctx in
        emit_op1 ctx pointer "l" "loadl" addr;
        let inner =
          match resolve_ty ty with
          | TPointer inner -> inner
          | _ -> Diagnostic.ice "MIR deref on non pointer"
        in
        project pointer inner rest
    | Mir.Field field :: rest ->
        let struct_name =
          match resolve_ty ty with
          | TStruct (name, _) -> name
          | _ -> Diagnostic.ice "MIR field on non struct"
        in
        let fields = Symbol.Table.find ctx.structs (Qname.key struct_name) in
        let field_ty = List.nth fields field in
        let addr =
          offset_addr ctx addr (field_offset ctx.structs fields field)
        in
        project addr field_ty rest
    | Mir.Index index_operand :: rest ->
        let element, storage =
          match resolve_ty ty with
          | TArray (element, _) -> (element, addr)
          | TSlice element -> (element, load_slice_ptr ctx addr)
          | TPointer element ->
              let pointer = fresh ctx in
              emit_op1 ctx pointer "l" "loadl" addr;
              (element, pointer)
          | _ -> Diagnostic.ice "MIR index on non indexed place"
        in
        let addr = emit_mir_index_addr mctx storage element index_operand in
        project addr element rest
  in
  let base_ty = mir_base_ty mctx place.Mir.base in
  let projections = List.rev place.Mir.projections in
  match place.Mir.base with
  | Mir.Global name -> project ("$" ^ name) base_ty projections
  | Mir.Local id -> (
      match (mctx.bindings.(id), projections) with
      | Memory addr, projections -> project addr base_ty projections
      | Value pointer, Mir.Deref :: rest -> (
          match resolve_ty base_ty with
          | TPointer element -> project pointer element rest
          | _ -> Diagnostic.ice "MIR deref on non pointer")
      | Value pointer, Mir.Index index :: rest -> (
          match resolve_ty base_ty with
          | TPointer element ->
              let addr = emit_mir_index_addr mctx pointer element index in
              project addr element rest
          | _ -> Diagnostic.ice "MIR index on non pointer")
      | Value _, [] -> Diagnostic.ice "value local used as an address"
      | Value _, Mir.Field _ :: _ ->
          Diagnostic.ice "MIR projection on scalar local")

let emit_mir_unary ctx (op : Mir.unop) operand ty =
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

let emit_mir_value mctx (value : Mir.value) =
  let ctx = mctx.qbe in
  match value.Mir.desc with
  | Mir.Use operand -> emit_mir_operand mctx operand
  | Mir.Unary (op, operand) ->
      emit_mir_unary ctx op (emit_mir_operand mctx operand) value.Mir.ty
  | Mir.Binary (op, left, right) ->
      let left_value = emit_mir_operand mctx left in
      let right_value = emit_mir_operand mctx right in
      emit_arith_binop ctx op ~result_ty:value.Mir.ty ~operand_ty:left.Mir.ty
        ~count_ty:right.Mir.ty left_value right_value
  | Mir.Cast operand ->
      emit_cast ctx (emit_mir_operand mctx operand) operand.Mir.ty value.Mir.ty
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
  | Mir.SizeOf ty -> string_of_int (ty_size ctx.structs ty)

let emit_value_into (mctx : mir_ctx) (destination : Mir.place)
    (destination_ty : ty) (value : string) =
  match direct_value_binding mctx destination with
  | Some binding ->
      emit_op1 mctx.qbe binding (qbe_ty destination_ty) "copy" value
  | None ->
      let destination_addr, destination_ty = emit_mir_place mctx destination in
      if is_aggregate destination_ty then
        emit_aggregate_copy mctx.qbe destination_addr value
          (ty_size mctx.qbe.structs destination_ty)
      else emit_store mctx.qbe (qbe_store destination_ty) value destination_addr

let emit_mir_assign mctx destination (value : Mir.value) =
  let ctx = mctx.qbe in
  match value.Mir.desc with
  | Mir.Use { Mir.desc = Mir.Const Mir.Undef; _ } -> ()
  | Mir.Use { Mir.desc = Mir.Const (Mir.Str content); _ } ->
      let destination_addr, _ = emit_mir_place mctx destination in
      emit_string_into ctx destination_addr content
  | Mir.Use { Mir.desc = Mir.Const Mir.Zero; _ } ->
      let destination_ty = value.Mir.ty in
      if is_aggregate destination_ty then begin
        let destination_addr, _ = emit_mir_place mctx destination in
        emit_zero_into ctx destination_addr destination_ty
      end
      else emit_value_into mctx destination destination_ty "0"
  | _ ->
      let result = emit_mir_value mctx value in
      emit_value_into mctx destination value.Mir.ty result

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
  let element =
    match resolve_ty destination_ty with
    | TSlice element -> element
    | _ -> Diagnostic.ice "MIR slice result has invalid type"
  in
  let lo_value = widened_operand mctx lo in
  let hi_value = widened_operand mctx hi in
  let pointer = emit_mir_index_addr mctx storage element lo in
  let length = fresh ctx in
  emit ctx "%s =l sub %s, %s\n" length hi_value lo_value;
  emit ctx "storel %s, %s\n" pointer destination_addr;
  emit ctx "storel %s, %s\n" length (offset_addr ctx destination_addr 8)

(* The condition is true when the check fails so it feeds the branch to the panic block *)
let emit_mir_check_condition mctx = function
  | Mir.Bounds (index, length) ->
      bounds_condition mctx.qbe
        (widened_operand mctx index)
        (widened_operand mctx length)
  | Mir.SliceBounds (lo, hi, length) ->
      slice_bounds_condition mctx.qbe (widened_operand mctx lo)
        (widened_operand mctx hi)
        (widened_operand mctx length)
  | Mir.Null pointer -> null_condition mctx.qbe (emit_mir_operand mctx pointer)
  | Mir.DivZero divisor ->
      div_zero_condition mctx.qbe
        (emit_mir_operand mctx divisor)
        (qbe_ty divisor.Mir.ty)
  | Mir.NegativeShift count ->
      negative_shift_condition mctx.qbe
        (emit_mir_operand mctx count)
        (qbe_ty count.Mir.ty)

let emit_mir_panic mctx span check =
  let ctx = mctx.qbe in
  let site = Panic_table.record ctx.panics span in
  (match check with
  | Mir.Bounds (index, length) ->
      emit ctx "call $ripe_panic_bounds(w %d, l %s, l %s)\n" site
        (widened_operand mctx index)
        (widened_operand mctx length)
  | Mir.SliceBounds (lo, hi, length) ->
      emit ctx "call $ripe_panic_slice_bounds(w %d, l %s, l %s, l %s)\n" site
        (widened_operand mctx lo) (widened_operand mctx hi)
        (widened_operand mctx length)
  | Mir.Null _ -> emit ctx "call $ripe_panic_null(w %d)\n" site
  | Mir.DivZero _ -> emit ctx "call $ripe_panic_divzero(w %d)\n" site
  | Mir.NegativeShift _ -> emit ctx "call $ripe_panic_shift(w %d)\n" site);
  emit ctx "hlt\n"

let mir_callee mctx = function
  | Mir.Direct name -> "$" ^ name
  | Mir.Indirect operand -> emit_mir_operand mctx operand

let qbe_abi_ty ctx ty =
  match resolve_ty ty with
  | TStruct (name, _) -> ":" ^ qbe_struct_name ctx name
  | (TArray _ | TSlice _ | TStr) as aggregate -> (
      match Hashtbl.find_opt ctx.abi_types aggregate with
      | Some name -> ":" ^ name
      | None ->
          let id = !(ctx.next_abi_type) in
          incr ctx.next_abi_type;
          let name = Printf.sprintf "RipeAbi%d" id in
          Hashtbl.add ctx.abi_types aggregate name;
          Printf.bprintf ctx.abi_defs "type :%s = { %s }\n" name
            (qbe_ext_ty (qbe_struct_name ctx) aggregate);
          ":" ^ name)
  | _ -> qbe_ty ty

let qbe_call_ty ctx kind ty =
  match kind with
  | Mir.External when is_aggregate ty -> qbe_abi_ty ctx ty
  | _ -> qbe_ty ty

let emit_mir_call mctx (call : Mir.call) =
  let ctx = mctx.qbe in
  let hidden_result =
    match (call.Mir.destination, call.Mir.return_ty, call.Mir.kind) with
    | Some destination, return_ty, Mir.Internal when is_aggregate return_ty ->
        Some (emit_mir_place mctx destination |> fst)
    | _ -> None
  in
  let args =
    List.map
      (fun (operand : Mir.operand) ->
        Printf.sprintf "%s %s"
          (qbe_call_ty ctx call.Mir.kind operand.Mir.ty)
          (emit_mir_operand mctx operand))
      call.Mir.args
  in
  let args =
    match hidden_result with
    | Some destination -> Printf.sprintf "l %s" destination :: args
    | None -> args
  in
  let args =
    match call.Mir.variadic_start with
    | None -> args
    | Some count -> List.take count args @ ("..." :: List.drop count args)
  in
  let callee = mir_callee mctx call.Mir.callee in
  match (call.Mir.destination, call.Mir.return_ty, call.Mir.kind) with
  | None, _, _ -> emit ctx "call %s(%s)\n" callee (String.concat ", " args)
  | Some _, return_ty, Mir.Internal when is_aggregate return_ty ->
      emit ctx "call %s(%s)\n" callee (String.concat ", " args)
  | Some destination, return_ty, _ ->
      let result = fresh ctx in
      emit ctx "%s =%s call %s(%s)\n" result
        (qbe_call_ty ctx call.Mir.kind return_ty)
        callee (String.concat ", " args);
      emit_value_into mctx destination return_ty result

let emit_mir_statement mctx (statement : Mir.statement) =
  match statement.Mir.desc with
  | Mir.Assign (destination, value) -> emit_mir_assign mctx destination value
  | Mir.Call call -> emit_mir_call mctx call
  | Mir.Slice (destination, source, lo, hi) ->
      emit_mir_slice mctx destination source lo hi

let emit_mir_terminator mctx (terminator : Mir.terminator) =
  let ctx = mctx.qbe in
  match terminator.Mir.desc with
  | Mir.Jump target -> emit_jmp ctx (mir_block_label ctx target)
  | Mir.Branch (condition, yes, no) ->
      emit_jnz ctx
        (emit_mir_operand mctx condition)
        (mir_block_label ctx yes) (mir_block_label ctx no)
  | Mir.Assert (check, ok, fail) ->
      emit_jnz ctx
        (emit_mir_check_condition mctx check)
        (mir_block_label ctx fail) (mir_block_label ctx ok)
  | Mir.Panic check -> emit_mir_panic mctx terminator.Mir.span check
  | Mir.ReturnValue None -> put ctx "ret\n"
  | Mir.ReturnValue (Some returned) ->
      let returned = emit_mir_operand mctx returned in
      put ctx "ret ";
      put ctx returned;
      put_char ctx '\n'
  | Mir.Unreachable -> put ctx "hlt\n"

let analyze_local_usage (func : Mir.func) =
  let address_taken = Array.make (Array.length func.Mir.locals) false in
  let defined = Array.make (Array.length func.Mir.locals) false in
  let mark_defined (place : Mir.place) =
    match (place.Mir.base, place.Mir.projections) with
    | Mir.Local id, [] -> defined.(id) <- true
    | Mir.Local _, _ | Mir.Global _, _ -> ()
  in
  let mark_statement (statement : Mir.statement) =
    match statement.Mir.desc with
    | Mir.Assign (destination, value) ->
        begin match value.Mir.desc with
        | Mir.Use { Mir.desc = Mir.Const Mir.Undef; _ } -> ()
        | Mir.AddressOf { Mir.base = Mir.Local id; projections = []; _ } ->
            mark_defined destination;
            address_taken.(id) <- true
        | _ -> mark_defined destination
        end
    | Mir.Call call -> Option.iter mark_defined call.Mir.destination
    | Mir.Slice (destination, _, _, _) -> mark_defined destination
  in
  let mark_block (block : Mir.block) =
    List.iter mark_statement block.Mir.statements
  in
  Array.iter mark_block func.Mir.blocks;
  List.iter (fun id -> defined.(id) <- true) func.Mir.params;
  { address_taken; defined }

let can_bind_value (local : Mir.local) =
  match resolve_ty local.Mir.ty with
  | TInt _ | TFloat _ | TBool | TChar | TEnum _ -> true
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ -> true
  | _ -> false

(* Keeps names readable in the generated IL and adds suffixes when needed *)
let bind_mir_locals (ctx : ctx) (func : Mir.func) =
  let usage = analyze_local_usage func in
  Array.mapi
    (fun id (local : Mir.local) ->
      let base =
        match local.Mir.name with Some n -> n | None -> "m" ^ string_of_int id
      in
      let slot =
        if Hashtbl.mem ctx.used_slots base || spelled_like_temp base then
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
    func.Mir.locals

let emit_mir_func ctx global_types (func : Mir.func) =
  Panic_table.enter_func ctx.panics func.Mir.source_name;
  ctx.tmp := 0;
  Hashtbl.clear ctx.used_slots;
  let bindings = bind_mir_locals ctx func in
  (* The result local lives in the caller's storage so it borrows the hidden pointer *)
  let result_tmp =
    match func.Mir.result with
    | None -> None
    | Some id ->
        let tmp = fresh ctx in
        bindings.(id) <- Memory tmp;
        Some tmp
  in
  let params =
    List.map
      (fun id ->
        let local = func.Mir.locals.(id) in
        let tmp = fresh ctx in
        (id, local.Mir.ty, tmp))
      func.Mir.params
  in
  let params_text =
    (match result_tmp with None -> [] | Some tmp -> [ "l " ^ tmp ])
    @ List.map (fun (_, ty, tmp) -> qbe_ty ty ^ " " ^ tmp) params
  in
  let is_main = func.Mir.entry_point && func.Mir.return_ty = TInt I32 in
  let return_text =
    if
      func.Mir.return_ty = TUnit
      || func.Mir.return_ty = TNever
      || func.Mir.result <> None
    then ""
    else qbe_abi_ty ctx func.Mir.return_ty ^ " "
  in
  emit ctx "%sfunction %s$%s(%s) {\n"
    (if is_main || func.Mir.public then "export " else "")
    return_text func.Mir.name
    (String.concat ", " params_text);
  emit_label ctx "@start";
  Array.iteri
    (fun id (local : Mir.local) ->
      match bindings.(id) with
      | Value _ -> ()
      | Memory slot ->
          if Some id <> func.Mir.result then
            emit_op1 ctx slot "l"
              (alloc_instr ctx.structs local.Mir.ty)
              (string_of_int (ty_size ctx.structs local.Mir.ty)))
    func.Mir.locals;
  List.iter
    (fun (id, ty, tmp) ->
      match bindings.(id) with
      | Value binding ->
          emit_op1 ctx binding (qbe_ty ty) "copy" (narrow_int_to ctx tmp ty)
      | Memory slot ->
          if is_aggregate ty then
            emit_aggregate_copy ctx slot tmp (ty_size ctx.structs ty)
          else emit_store ctx (qbe_store ty) tmp slot)
    params;
  let mctx = { qbe = ctx; func; bindings; global_types } in
  Array.iteri
    (fun id (block : Mir.block) ->
      if id > 0 then emit_label ctx (mir_block_label ctx id);
      List.iter (emit_mir_statement mctx) block.Mir.statements;
      match block.Mir.terminator with
      | Some terminator -> emit_mir_terminator mctx terminator
      | None -> Diagnostic.ice "unterminated verified MIR block")
    func.Mir.blocks;
  emit ctx "}\n\n"

let rec emit_mir_global_fields ctx expected = function
  | Mir.GlobalConst (Mir.Zero, ty) ->
      Printf.sprintf "z %d" (ty_size ctx.structs ty)
  | Mir.GlobalConst (Mir.Undef, ty) ->
      Printf.sprintf "z %d" (ty_size ctx.structs ty)
  | Mir.GlobalConst (Mir.Str content, _) ->
      let label = intern_string ctx content in
      Printf.sprintf "l %s, l %d" label (String.length content)
  | Mir.GlobalConst (constant, ty) ->
      Printf.sprintf "%s %s"
        (qbe_ext_ty (qbe_struct_name ctx) ty)
        (emit_mir_constant ctx ty constant)
  | Mir.GlobalAddress name -> "l $" ^ name
  | Mir.GlobalArray values ->
      let element =
        match resolve_ty expected with
        | TArray (element, _) -> element
        | _ -> Diagnostic.ice "MIR global array has invalid type"
      in
      String.concat ", " (List.map (emit_mir_global_fields ctx element) values)
  | Mir.GlobalStruct values ->
      let fields =
        match resolve_ty expected with
        | TStruct (name, _) -> Symbol.Table.find ctx.structs (Qname.key name)
        | _ -> Diagnostic.ice "MIR global struct has invalid type"
      in
      let offset = ref 0 in
      let parts = ref [] in
      List.iter
        (fun (field, value) ->
          let field_ty = List.nth fields field in
          let aligned = align_to !offset (ty_align ctx.structs field_ty) in
          if aligned > !offset then
            parts := Printf.sprintf "z %d" (aligned - !offset) :: !parts;
          parts := emit_mir_global_fields ctx field_ty value :: !parts;
          offset := aligned + ty_size ctx.structs field_ty)
        values;
      let total = ty_size ctx.structs expected in
      if total > !offset then
        parts := Printf.sprintf "z %d" (total - !offset) :: !parts;
      String.concat ", " (List.rev !parts)

let emit_mir_global ctx (global : Mir.global) =
  let align = ty_align ctx.structs global.Mir.ty in
  let export = if global.Mir.public then "export " else "" in
  match global.Mir.init with
  | None ->
      emit ctx "%sdata $%s = align %d { z %d }\n" export global.Mir.name align
        (ty_size ctx.structs global.Mir.ty)
  | Some value ->
      emit ctx "%sdata $%s = align %d { %s }\n" export global.Mir.name align
        (emit_mir_global_fields ctx global.Mir.ty value)

let emit_mir ~source_of (program : Mir.program) =
  let structs = Symbol.Table.create 8 in
  let struct_names = Symbol.Table.create 8 in
  List.iter
    (fun (decl : Mir.struct_decl) ->
      let key = Qname.key decl.Mir.name in
      Symbol.Table.add structs key decl.Mir.fields;
      let name =
        if decl.Mir.local then
          Printf.sprintf "_Rlocal%d_%d"
            (Symbol.module_id_of_key key)
            (Symbol.id_of_key key)
        else Qname.show decl.Mir.name
      in
      Symbol.Table.add struct_names key name)
    program.Mir.structs;
  let global_types = Hashtbl.create 16 in
  List.iter
    (fun (global : Mir.global) ->
      Hashtbl.add global_types global.Mir.name global.Mir.ty)
    program.Mir.globals;
  let ctx =
    {
      structs;
      struct_names;
      panics = Panic_table.create ~source_of;
      used_slots = Hashtbl.create 16;
      buf = ref (Buffer.create 1024);
      strings = ref [];
      abi_types = Hashtbl.create 8;
      abi_defs = Buffer.create 128;
      next_abi_type = ref 0;
      tmp = ref 0;
      temp_names = ref [||];
      block_labels = ref [||];
      str_ctr = ref 0;
    }
  in
  List.iter
    (fun (decl : Mir.struct_decl) ->
      emit_struct_type ctx decl.Mir.name decl.Mir.fields)
    program.Mir.structs;
  if not (List.is_empty program.Mir.structs) then emit ctx "\n";
  let types_end = Buffer.length !(ctx.buf) in
  List.iter (emit_mir_global ctx) program.Mir.globals;
  if not (List.is_empty program.Mir.globals) then emit ctx "\n";
  List.iter (emit_mir_func ctx global_types) program.Mir.functions;
  (* A program with no checks emits neither table and the runtime declares both weak so it still links *)
  (match Panic_table.sites ctx.panics with
  | [] -> ()
  | sites ->
      let chunk s = Printf.sprintf "b \"%s\", b 0" (escape_data_string s) in
      let entry (site : Panic_table.site) =
        Printf.sprintf "w %d, w %d, w %d, w %d" site.Panic_table.file
          site.Panic_table.line site.Panic_table.col site.Panic_table.func
      in
      emit ctx "export data $ripe_panic_strtab = { %s }\n"
        (String.concat ", " (List.map chunk (Panic_table.strings ctx.panics)));
      emit ctx "export data $ripe_panic_sites = align 4 { %s }\n"
        (String.concat ", " (List.map entry sites)));
  List.iter
    (fun (label, content) -> emit_string_data ctx label content)
    (List.rev !(ctx.strings));
  let out = !(ctx.buf) in
  if Buffer.length ctx.abi_defs = 0 then Buffer.contents out
  else
    Buffer.sub out 0 types_end
    ^ Buffer.contents ctx.abi_defs
    ^ Buffer.sub out types_end (Buffer.length out - types_end)
