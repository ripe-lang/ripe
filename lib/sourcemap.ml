(* SPDX-License-Identifier: Apache-2.0 *)

(* This turns byte offsets into line and col positions *)

type t = { src : string; line_starts : int array }

(* The source gets scanned once to save the line starts *)
let create src =
  let len = String.length src in
  let starts = ref [ 0 ] in
  for i = 0 to len - 1 do
    if src.[i] = '\n' && i + 1 < len then starts := (i + 1) :: !starts
  done;
  { src; line_starts = Array.of_list (List.rev !starts) }

(* This binary search finds the right most entry and needs no equal case *)
let rec search starts pos lo hi =
  if lo >= hi then lo
  else
    let mid = lo + ((hi - lo + 1) / 2) in
    if starts.(mid) <= pos then search starts pos mid hi
    else search starts pos lo (mid - 1)

let src t = t.src

let find t pos =
  let i = search t.line_starts pos 0 (Array.length t.line_starts - 1) in
  (i + 1, pos - t.line_starts.(i) + 1)

(* The byte offsets of the line holding pos without its newline *)
let line_bounds t pos =
  let i = search t.line_starts pos 0 (Array.length t.line_starts - 1) in
  let start = t.line_starts.(i) in
  let stop =
    if i + 1 < Array.length t.line_starts then t.line_starts.(i + 1) - 1
    else
      let len = String.length t.src in
      if len > start && t.src.[len - 1] = '\n' then len - 1 else len
  in
  (start, stop)
