(* SPDX-License-Identifier: Apache-2.0 *)

(* Reference *)
(* 1. System V AMD64 psABI https://gitlab.com/x86-psABIs/x86-64-ABI *)

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

let lower_main func =
  let open X86_ir in
  [ Mov_imm (W32, Rax, return_constant func); Ret ]

let assemble blocks =
  let encoder = X86_encode.create () in

  let place labels (name, instrs) =
    let at = X86_encode.offset encoder in
    List.iter (X86_encode.instr encoder) instrs;
    (name, at) :: labels
  in

  let labels = List.fold_left place [] blocks in
  (X86_encode.finish encoder ~labels, List.rev labels)

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
