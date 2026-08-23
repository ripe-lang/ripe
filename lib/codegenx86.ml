(* SPDX-License-Identifier: Apache-2.0 *)

(* Reference *)
(* 1. Intel 64 and IA-32 Architectures Software Developer Manuals https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html *)
(* 2. System V AMD64 psABI https://gitlab.com/x86-psABIs/x86-64-ABI *)

type reg =
  | Rax
  | Rcx
  | Rdx
  | Rbx
  | Rsp
  | Rbp
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type width = W32 | W64

type instr =
  | Mov_imm of width * reg * int64
  | Mov_reg of width * reg * reg
  | Call of string
  | Ret
  | Syscall

let reg_index = function
  | Rax -> 0
  | Rcx -> 1
  | Rdx -> 2
  | Rbx -> 3
  | Rsp -> 4
  | Rbp -> 5
  | Rsi -> 6
  | Rdi -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15

(* Extended registers carry their fourth index bit in REX [1] *)
let is_extended register = reg_index register > 7

module Labels = Hashtbl.Make (String)

type fixup = { patch_at : int; target : string }
type t = { bytes : Buffer.t; mutable fixups : fixup list }

let min_rel32 = -0x8000_0000
let max_rel32 = 0x7fff_ffff

(* The imm32 encoding accepts signed and unsigned callers [1] *)
let min_imm32 = -0x8000_0000L
let max_imm32 = 0xffff_ffffL
let create ?(size = 64) () = { bytes = Buffer.create size; fixups = [] }
let offset encoder = Buffer.length encoder.bytes

let byte encoder value =
  Buffer.add_char encoder.bytes (Char.chr (value land 0xff))

let le32 encoder value =
  for shift = 0 to 3 do
    byte encoder (value asr (shift * 8))
  done

let le64 encoder value =
  le32 encoder (Int64.to_int value);
  le32 encoder (Int64.to_int (Int64.shift_right_logical value 32))

(* REX is only emitted when a width or extended register bit is actually set [1] *)
let rex encoder ~w ~r ~b =
  let bits =
    (if w then 8 else 0) lor (if r then 4 else 0) lor if b then 1 else 0
  in
  if bits <> 0 then byte encoder (0x40 lor bits)

(* ModRM packs the addressing mode and two register fields into one byte [1] *)
let modrm encoder ~md ~reg ~rm =
  byte encoder ((md lsl 6) lor ((reg land 7) lsl 3) lor (rm land 7))

(* Register direct mode avoids special stack and frame register meanings [1] *)
let register_direct = 3

(* Instruction encodings follow the SDM instruction pages [1] *)
let instr encoder instruction =
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

let finish encoder ~labels =
  let image = Buffer.to_bytes encoder.bytes in
  let table = Labels.create (List.length labels) in
  List.iter (fun (name, at) -> Labels.replace table name at) labels;
  let patch fixup =
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

exception Unsupported of string

let unsupported message = raise (Unsupported message)

let fits_in_int32 value =
  Int64.equal value (Int64.of_int32 (Int64.to_int32 value))

let return_constant func =
  let open Mir in
  if Array.length func.blocks <> 1 then
    unsupported "the x86 backend cannot compile more than one block yet";

  let block = func.blocks.(0) in
  if not (List.is_empty block.statements) then
    unsupported "the x86 backend cannot compile statements yet";

  match block.terminator with
  | Some { desc = ReturnValue (Some { desc = Const (Int value); _ }); _ } ->
      if not (fits_in_int32 value) then
        unsupported
          "the x86 backend cannot compile a literal outside 32 bit range yet";
      value
  | _ -> unsupported "the x86 backend only compiles a returned integer literal"

let lower_main func = [ Mov_imm (W32, Rax, return_constant func); Ret ]

let assemble blocks =
  let encoder = create () in

  let place labels (name, instrs) =
    let at = offset encoder in
    List.iter (instr encoder) instrs;
    (name, at) :: labels
  in

  let labels = List.fold_left place [] blocks in
  (finish encoder ~labels, List.rev labels)

let find_main program =
  let open Mir in
  match List.find_opt (fun func -> func.entry_point) program.functions with
  | Some func -> func
  | None -> unsupported "the x86 backend needs a main function"

let emit_mir ~source_of:_ program =
  let open Mir in
  if not (List.is_empty program.globals) then
    unsupported "the x86 backend cannot compile globals yet";
  if List.compare_length_with program.functions 1 <> 0 then
    unsupported "the x86 backend cannot compile more than one function yet";

  let main = find_main program in
  if main.return_ty <> Types.TInt Types.I32 then
    unsupported "the x86 backend needs main to return i32";

  let text, globals = assemble [ (main.name, lower_main main) ] in
  Elf64.object_file ~text ~globals
