(* SPDX-License-Identifier: Apache-2.0 *)

(* Reference *)
(* 1. Intel 64 and IA-32 Architectures Software Developer Manuals https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html *)

module Labels = Hashtbl.Make (String)

type fixup = { patch_at : int; target : string }
type t = { bytes : Buffer.t; mutable fixups : fixup list }

let min_rel32 = -0x8000_0000
let max_rel32 = 0x7fff_ffff

(* The imm32 encoding accepts signed and unsigned callers [1] *)
let min_imm32 = -0x8000_0000L
let max_imm32 = 0xffff_ffffL
let create ?(size = 64) () : t = { bytes = Buffer.create size; fixups = [] }
let offset (encoder : t) : int = Buffer.length encoder.bytes

let byte (encoder : t) (value : int) : unit =
  Buffer.add_char encoder.bytes (Char.chr (value land 0xff))

let le32 (encoder : t) (value : int) : unit =
  for shift = 0 to 3 do
    byte encoder (value asr (shift * 8))
  done

let le64 (encoder : t) (value : int64) : unit =
  le32 encoder (Int64.to_int value);
  le32 encoder (Int64.to_int (Int64.shift_right_logical value 32))

(* REX is only emitted when a width or extended register bit is actually set [1] *)
let rex (encoder : t) ~(w : bool) ~(r : bool) ~(b : bool) : unit =
  let bits =
    (if w then 8 else 0) lor (if r then 4 else 0) lor if b then 1 else 0
  in
  if bits <> 0 then byte encoder (0x40 lor bits)

(* ModRM packs the addressing mode and two register fields into one byte [1] *)
let modrm (encoder : t) ~(md : int) ~(reg : int) ~(rm : int) : unit =
  byte encoder ((md lsl 6) lor ((reg land 7) lsl 3) lor (rm land 7))

(* Register direct mode avoids special stack and frame register meanings [1] *)
let register_direct = 3

(* Instruction encodings follow the SDM instruction pages [1] *)
let instr (encoder : t) (instruction : X86_ir.instr) : unit =
  let open X86_ir in
  match instruction with
  (* Immediate width follows the operand size [1] *)
  | Mov_imm (width, dst, value) ->
      if
        width = W32
        && (Int64.compare value min_imm32 < 0
           || Int64.compare value max_imm32 > 0)
      then Diagnostic.ice (Printf.sprintf "%Ld does not fit in an imm32" value);
      rex encoder ~w:(width = W64) ~r:false ~b:(is_extended dst);
      byte encoder (0xb8 lor (reg_index dst land 7));
      if width = W64 then le64 encoder value
      else le32 encoder (Int64.to_int value)
  (* The source occupies ModRM reg and the destination occupies r/m [1] *)
  | Mov_reg (width, dst, src) ->
      rex encoder ~w:(width = W64) ~r:(is_extended src) ~b:(is_extended dst);
      byte encoder 0x89;
      modrm encoder ~md:register_direct ~reg:(reg_index src) ~rm:(reg_index dst)
  (* Calls store a signed displacement from the next instruction [1] *)
  | Call target ->
      byte encoder 0xe8;
      encoder.fixups <- { patch_at = offset encoder; target } :: encoder.fixups;
      le32 encoder 0
  | Ret -> byte encoder 0xc3
  | Syscall ->
      byte encoder 0x0f;
      byte encoder 0x05

let finish (encoder : t) ~(labels : (string * int) list) : string =
  let image = Buffer.to_bytes encoder.bytes in
  let table = Labels.create (List.length labels) in
  List.iter (fun (name, at) -> Labels.replace table name at) labels;
  let patch (fixup : fixup) =
    match Labels.find_opt table fixup.target with
    | None -> Diagnostic.ice ("no label named " ^ fixup.target)
    | Some target ->
        (* The displacement starts after the four byte immediate [1] *)
        let relative = target - (fixup.patch_at + 4) in
        if relative < min_rel32 || relative > max_rel32 then
          Diagnostic.ice
            (Printf.sprintf "displacement to %s is %d bytes" fixup.target
               relative);
        Bytes.set_int32_le image fixup.patch_at (Int32.of_int relative)
  in
  List.iter patch encoder.fixups;
  Bytes.to_string image
