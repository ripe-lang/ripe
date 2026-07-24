(* SPDX-License-Identifier: GPL-2.0-only *)

type t = { lo : int; hi : int }

let pp fmt { lo; hi } = Format.fprintf fmt "(%d,%d)" lo hi
let show s = Format.asprintf "%a" pp s
let dummy = { lo = 0; hi = 0 }
