(* SPDX-License-Identifier: Apache-2.0 *)

(* Converts byte offsets to line/col positions *)

type t = {
  base : int; (* Where this file starts in the global offset space *)
  src : string;
  line_starts : int array; (* Sorted byte offsets where each line begins *)
}

(* Only scan source text once to save the line start *)
let create ~base src =
  let len = String.length src in
  let starts = ref [ 0 ] in
  for i = 0 to len - 1 do
    if src.[i] = '\n' && i + 1 < len then starts := (i + 1) :: !starts
  done;
  { base; src; line_starts = Array.of_list (List.rev !starts) }

(* Binary search for the right most entry (the equal case not needed) *)
let rec search starts pos lo hi =
  if lo >= hi then lo
  else
    let mid = lo + ((hi - lo + 1) / 2) in
    if starts.(mid) <= pos then search starts pos mid hi
    else search starts pos lo (mid - 1)

let src t = t.src

let line_count t =
  let length = String.length t.src in
  Array.length t.line_starts
  + if length > 0 && t.src.[length - 1] = '\n' then 1 else 0

(* Offsets are global so anything indexing into `src` has to come through here *)
let rel t pos = pos - t.base

let lookup t pos =
  let pos = rel t pos in
  let i = search t.line_starts pos 0 (Array.length t.line_starts - 1) in
  (i + 1, pos - t.line_starts.(i) + 1)

(* Byte offsets of the line containing pos with newline excluded *)
(* Takes a global offset and gives back indices into `src` *)
let line_bounds t pos =
  let i = search t.line_starts (rel t pos) 0 (Array.length t.line_starts - 1) in
  let start = t.line_starts.(i) in
  let stop =
    if i + 1 < Array.length t.line_starts then t.line_starts.(i + 1) - 1
    else
      let len = String.length t.src in
      if len > start && t.src.[len - 1] = '\n' then len - 1 else len
  in
  (start, stop)
