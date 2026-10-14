(* SPDX-License-Identifier: Apache-2.0 *)

(* A relocatable object holding one text section and its global symbols *)
val object_file : text:string -> globals:(string * int) list -> string
