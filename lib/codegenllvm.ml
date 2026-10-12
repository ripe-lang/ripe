(* SPDX-License-Identifier: Apache-2.0 *)

(* https://llvm.org/docs/LangRef.html *)

let emit_entry_wrapper llctx llmod builder callee callee_ty =
  let i32 = Llvm.i32_type llctx in
  let main = Llvm.define_function "main" (Llvm.function_type i32 [||]) llmod in
  Llvm.position_at_end (Llvm.entry_block main) builder;
  ignore (Llvm.build_call callee_ty callee [||] "" builder);
  ignore (Llvm.build_ret (Llvm.const_int i32 0) builder)

let emit (program : Mir.program) =
  let llctx = Llvm.create_context () in
  let llmod = Llvm.create_module llctx "ripe" in
  let builder = Llvm.builder llctx in
  let func_ty = Llvm.function_type (Llvm.void_type llctx) [||] in
  List.iter
    (fun (func : Mir.func) ->
      match func.Mir.blocks with
      | [|
       { statements = []; terminator = Some { desc = ReturnValue None; _ } };
      |]
        when func.Mir.entry_point ->
          let llfunc = Llvm.define_function func.Mir.name func_ty llmod in
          Llvm.position_at_end (Llvm.entry_block llfunc) builder;
          ignore (Llvm.build_ret_void builder);
          emit_entry_wrapper llctx llmod builder llfunc func_ty
      | _ -> Diagnostic.ice "LLVM only lowers an empty main yet")
    program.Mir.functions;
  llmod
