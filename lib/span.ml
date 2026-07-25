(* SPDX-License-Identifier: GPL-2.0-only *)

type file_id = int
type t = { file : file_id; lo : int; hi : int }

let make (file : file_id) (lo : int) (hi : int) : t = { file; lo; hi }

let pp (fmt : Format.formatter) ({ lo; hi; _ } : t) : unit =
  Format.fprintf fmt "(%d,%d)" lo hi

let show (s : t) : string = Format.asprintf "%a" pp s
let dummy : t = make (-1) 0 0
