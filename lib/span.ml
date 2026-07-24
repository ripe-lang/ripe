(* SPDX-License-Identifier: GPL-2.0-only *)

type file_id = int
type t = { file : file_id; lo : int; hi : int }

let make file lo hi = { file; lo; hi }
let pp fmt { lo; hi; _ } = Format.fprintf fmt "(%d,%d)" lo hi
let show s = Format.asprintf "%a" pp s
let dummy = make (-1) 0 0
