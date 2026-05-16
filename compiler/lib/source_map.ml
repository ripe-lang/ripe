(* SPDX-License-Identifier: GPL-2.0-only *)

(* Converts byte offsets to line/col positions *)

type t = {
  src : string;
  line_starts : int array; (* sorted byte offsets where each line begins *)
}

(* TODO(c351): Wire up in main.ml once parser is done *)
(* Only scan source text once to save the line start *)
let create src =
  let len = String.length src in
  let starts = ref [ 0 ] in
  for i = 0 to len - 1 do
    if src.[i] = '\n' && i + 1 < len then starts := (i + 1) :: !starts
  done;
  { src; line_starts = Array.of_list (List.rev !starts) }

(* Binary search for the right most entry (the equal case not needed) *)
let rec search starts pos lo hi =
  if lo >= hi then lo
  else
    let mid = lo + ((hi - lo + 1) / 2) in
    if starts.(mid) <= pos then search starts pos mid hi
    else search starts pos lo (mid - 1)

(* 1 based array index (line, col) *)
let lookup t pos =
  let i = search t.line_starts pos 0 (Array.length t.line_starts - 1) in
  (i + 1, pos - t.line_starts.(i) + 1)

(* A basic wrapper for lookup to get the span e.g.
   file.rp:1:5: type mismatch
     let x = "hello" + 5
             ^~~~~~~~~~~ *)
let span_to_locs t (span : Ast.span) =
  let start_line, start_col = lookup t span.lo in
  let end_line, end_col = lookup t span.hi in
  (start_line, start_col, end_line, end_col)
