(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let hex (bytes : string) : string =
  String.to_seq bytes
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat " "

let encode ?(labels = []) (instrs : Codegenx86.instr list) : unit =
  let encoder = Codegenx86.create () in
  List.iter (Codegenx86.instr encoder) instrs;
  print_string (hex (Codegenx86.finish encoder ~labels))

let%expect_test "mov imm32 needs no prefix for the low registers" =
  encode [ Codegenx86.Mov_imm (W32, Rax, 42L) ];
  [%expect {| b8 2a 00 00 00 |}]

let%expect_test "mov imm32 into an extended register sets REX.B" =
  encode [ Codegenx86.Mov_imm (W32, R8, 42L) ];
  [%expect {| 41 b8 2a 00 00 00 |}]

let%expect_test "mov imm64 sets REX.W and takes eight bytes" =
  encode [ Codegenx86.Mov_imm (W64, Rax, 42L) ];
  [%expect {| 48 b8 2a 00 00 00 00 00 00 00 |}]

let%expect_test "mov imm64 into an extended register sets REX.W and REX.B" =
  encode [ Codegenx86.Mov_imm (W64, R15, 42L) ];
  [%expect {| 49 bf 2a 00 00 00 00 00 00 00 |}]

let%expect_test "a wide imm64 keeps its high word" =
  encode [ Codegenx86.Mov_imm (W64, Rax, 0xdeadbeefcafebabeL) ];
  [%expect {| 48 b8 be ba fe ca ef be ad de |}]

let%expect_test "a negative imm64 fills every byte" =
  encode [ Codegenx86.Mov_imm (W64, R15, -1L) ];
  [%expect {| 49 bf ff ff ff ff ff ff ff ff |}]

let%expect_test "a negative imm32 fills four bytes" =
  encode [ Codegenx86.Mov_imm (W32, Rax, -1L) ];
  [%expect {| b8 ff ff ff ff |}]

let%expect_test "mov between low registers is two bytes" =
  encode [ Codegenx86.Mov_reg (W32, Rdi, Rax) ];
  [%expect {| 89 c7 |}]

let%expect_test "a 64 bit move sets REX.W" =
  encode [ Codegenx86.Mov_reg (W64, Rdi, Rax) ];
  [%expect {| 48 89 c7 |}]

let%expect_test "both operands extended sets REX.W and REX.R and REX.B" =
  encode [ Codegenx86.Mov_reg (W64, R9, R10) ];
  [%expect {| 4d 89 d1 |}]

let%expect_test "an extended source sets REX.R" =
  encode [ Codegenx86.Mov_reg (W32, Rdi, R8) ];
  [%expect {| 44 89 c7 |}]

let%expect_test "an extended destination sets REX.B" =
  encode [ Codegenx86.Mov_reg (W32, R8, Rdi) ];
  [%expect {| 41 89 f8 |}]

let%expect_test "ret and syscall have no operands" =
  encode [ Codegenx86.Ret; Codegenx86.Syscall ];
  [%expect {| c3 0f 05 |}]

let%expect_test "a call counts its displacement from the next instruction" =
  encode ~labels:[ ("target", 10) ] [ Codegenx86.Call "target" ];
  [%expect {| e8 05 00 00 00 |}]

let%expect_test "a backward call takes a negative displacement" =
  encode ~labels:[ ("target", 0) ] [ Codegenx86.Ret; Codegenx86.Call "target" ];
  [%expect {| c3 e8 fa ff ff ff |}]

let%expect_test "an unsigned imm32 reaches the top of the range" =
  encode [ Codegenx86.Mov_imm (W32, Rax, 0xffffffffL) ];
  [%expect {| b8 ff ff ff ff |}]

let%expect_test "a value too wide for an imm32 is refused" =
  (try encode [ Codegenx86.Mov_imm (W32, Rax, 0x1_0000_0000L) ]
   with Diagnostic.Errors ds ->
     List.iter
       (fun (d : Diagnostic.t) ->
         print_string (Option.value d.detail ~default:""))
       ds);
  [%expect {| 4294967296 does not fit in an imm32 |}]
