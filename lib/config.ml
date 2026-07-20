(* SPDX-License-Identifier: GPL-2.0-only *)

let qbe = match Sys.getenv_opt "QBE" with Some p -> p | None -> "qbe"
