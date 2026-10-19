(* SPDX-License-Identifier: Apache-2.0 *)

(* https://llvm.org/docs/LangRef.html *)

let emit_entry_wrapper llctx llmod builder callee callee_ty =
  let i32 = Llvm.i32_type llctx in
  let main = Llvm.define_function "main" (Llvm.function_type i32 [||]) llmod in
  Llvm.position_at_end (Llvm.entry_block main) builder;
  ignore (Llvm.build_call callee_ty callee [||] "" builder);
  ignore (Llvm.build_ret (Llvm.const_int i32 0) builder)

let emit program =
  let llctx = Llvm.create_context () in
  let llmod = Llvm.create_module llctx "ripe" in
  let builder = Llvm.builder llctx in
  let func_ty = Llvm.function_type (Llvm.void_type llctx) [||] in
  List.iter
    (fun func ->
      match func.Mir.func_blocks with
      | [|
       {
         block_stmts = [];
         block_term = Some { term_desc = ReturnValue None; _ };
       };
      |]
        when func.Mir.func_entry_point ->
          let llfunc = Llvm.define_function func.Mir.func_name func_ty llmod in
          Llvm.position_at_end (Llvm.entry_block llfunc) builder;
          ignore (Llvm.build_ret_void builder);
          emit_entry_wrapper llctx llmod builder llfunc func_ty
      | _ -> Diagnostic.ice "LLVM only lowers an empty main yet")
    program.Mir.prog_funcs;
  llmod

(* https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html
   https://github.com/llvm/llvm-project/blob/release/19.x/llvm/test/Bindings/OCaml/target.ml
   https://github.com/llvm/llvm-project/blob/release/19.x/llvm/bindings/ocaml/backends/backend_ocaml.c *)
let object_file llmod =
  Llvm_all_backends.initialize ();
  let triple = Llvm_target.Target.default_triple () in
  let machine =
    Llvm_target.TargetMachine.create ~triple
      ~reloc_mode:Llvm_target.RelocMode.PIC
      (Llvm_target.Target.by_triple triple)
  in
  Llvm.set_target_triple triple llmod;
  Llvm.set_data_layout
    (Llvm_target.DataLayout.as_string
       (Llvm_target.TargetMachine.data_layout machine))
    llmod;
  let buffer =
    Llvm_target.TargetMachine.emit_to_memory_buffer llmod ObjectFile machine
  in
  let bytes = Llvm.MemoryBuffer.as_string buffer in
  Llvm.MemoryBuffer.dispose buffer;
  bytes
