(* SPDX-License-Identifier: Apache-2.0 *)

type severity = Error | Warning | Note | Help
type span_label = { span : Ast.span; message : string }

type t = {
  severity : severity;
  headline : string;
  primary : Ast.span option; (* Caret snippet target *)
  primary_label : string option; (* Inline text after the caret *)
  labels : span_label list; (* Secondary snippets *)
  detail : string option; (* Verbatim block after the snippet *)
  suggestion : string option; (* Closing "help:" line *)
}

exception Errors of t list

let headline d = d.headline
let primary d = d.primary
let detail_of d = d.detail

let make severity headline =
  {
    severity;
    headline;
    primary = None;
    primary_label = None;
    labels = [];
    detail = None;
    suggestion = None;
  }

(* error: type mismatch *)
let error span fmt =
  Printf.ksprintf
    (fun headline -> { (make Error headline) with primary = Some span })
    fmt

(* warning: unused variable *)
let warning span headline = { (make Warning headline) with primary = Some span }

(* error: no source to point at *)
let error_no_span headline = make Error headline

(* ^~~~ expected i32, found bool *)
let label fmt =
  Printf.ksprintf (fun message d -> { d with primary_label = Some message }) fmt

(* ^ found = *)
let found text d = label "found %s" text d

(* ^ previous definition here *)
let secondary span message d = { d with labels = { span; message } :: d.labels }

(*   looked in this module *)
let detail s d = { d with detail = Some s }

(* help: add `;` here *)
let help s d = { d with suggestion = Some s }

(* error: internal compiler error *)
let internal ?span msg =
  let d =
    error_no_span "internal compiler error"
    |> detail (msg ^ "\n")
    |> help
         "this is a bug in ripec, please report it at \
          https://github.com/ripe-lang/ripe/issues"
  in
  match span with Some sp -> { d with primary = Some sp } | None -> d

let ice ?span msg = raise (Errors [ internal ?span msg ])

(* Where a pass dumps diagnostics and the edge drains it to render *)
type sink = { mutable pending : t list; mutable errors : bool }

let sink () = { pending = []; errors = false }

let emit s d =
  s.pending <- d :: s.pending;
  s.errors <- s.errors || d.severity = Error

let has_errors s = s.errors

(* Sorted into source order and ties keep emission order *)
let drain s =
  let pos d = match d.primary with Some sp -> Span.lo sp | None -> -1 in
  List.stable_sort (fun a b -> compare (pos a) (pos b)) (List.rev s.pending)

(* The next stage would report these all over again if the sink kept them *)
let take s =
  let all = drain s in
  s.pending <- [];
  s.errors <- false;
  all

(* Rendering *)

type ctx = { sm : Sourcemap.t; filename : string; color : bool }

let severity_word (severity : severity) =
  match severity with
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"
  | Help -> "help"

let severity_ansi (severity : severity) text =
  let color =
    match severity with
    | Error -> "\027[1;31m" (* red *)
    | Warning -> "\027[1;33m" (* yellow *)
    | Note -> "\027[1;36m" (* cyan *)
    | Help -> "\027[1;32m" (* green *)
  in
  color ^ text ^ "\027[0m" (* reset *)

let colored ctx sev s = if ctx.color then severity_ansi sev s else s

let severity_label color sev =
  if color then severity_ansi sev (severity_word sev) else severity_word sev

let tab_width = 8
let snippet_width = 100
let snippet_indent = 4
let ellipsis = "..."

(* UTF8 continuation bytes don't advance a column *)
let is_cont c = Char.code c land 0xc0 = 0x80

(* The cache must not keep old source files alive *)
module Columns = Ephemeron.K1.Make (struct
  type t = Sourcemap.t

  let equal a b = a == b

  (* The source text would make every cache hit scan the file again *)
  let hash sm =
    Hashtbl.hash (Sourcemap.rel sm 0, String.length (Sourcemap.src sm))
end)

let columns = Columns.create 16

let line_columns src start stop =
  (* The final newline still counts toward the EOF column *)
  let limit = min (String.length src) (stop + 1) in
  let cols = Array.make (limit - start + 1) 0 in
  for i = start to limit - 1 do
    let col = cols.(i - start) in
    let next =
      if src.[i] = '\t' then ((col / tab_width) + 1) * tab_width
      else if is_cont src.[i] then col
      else col + 1
    in
    cols.(i - start + 1) <- next
  done;
  cols

let line_cells src start stop cols =
  let byte = ref start in
  Array.init
    cols.(stop - start)
    (fun column ->
      while cols.(!byte - start + 1) <= column do
        incr byte
      done;
      if src.[!byte] = '\t' then " "
      else begin
        let first = !byte in
        incr byte;
        while !byte < stop && is_cont src.[!byte] do
          incr byte
        done;
        String.sub src first (!byte - first)
      end)

let cached_line sm start stop =
  let lines =
    match Columns.find_opt columns sm with
    | Some lines -> lines
    | None ->
        let lines = Hashtbl.create 16 in
        Columns.add columns sm lines;
        lines
  in
  match Hashtbl.find_opt lines start with
  | Some line -> line
  | None ->
      let src = Sourcemap.src sm in
      let cols = line_columns src start stop in
      let line = (cols, line_cells src start stop cols) in
      Hashtbl.add lines start line;
      line

(* ... + value + value + value ... *)
let window_of cells total caret_lo =
  let budget = snippet_width - snippet_indent in
  let start = max 0 (min (caret_lo - (budget / 2)) (total - budget)) in
  let edge = min total (start + budget) in
  let left = if start > 0 then ellipsis else "" in
  let right = if edge < total then ellipsis else "" in
  let lo = start + String.length left and hi = edge - String.length right in
  let shown =
    Array.sub cells lo (hi - lo) |> Array.to_list |> String.concat ""
  in
  (left ^ shown ^ right, start, edge)

(* Offsets here index into the raw source so they have to be file relative *)
let render_snippet ctx buf span label severity =
  let src = Sourcemap.src ctx.sm in
  let lo = Sourcemap.rel ctx.sm (Span.lo span) in
  let line_start, line_end = Sourcemap.line_bounds ctx.sm (Span.lo span) in
  let cols, cells = cached_line ctx.sm line_start line_end in
  let col pos = cols.(pos - line_start) in
  let caret_lo = col lo in
  let line, _ = Sourcemap.lookup ctx.sm (Span.lo span) in
  Printf.bprintf buf "  at %s:%d:%d\n" ctx.filename line (caret_lo + 1);

  let stop = ref (min line_end (lo + (4 * snippet_width) + 4)) in
  while !stop < line_end && is_cont src.[!stop] do
    incr stop
  done;
  let shown, offset, edge = window_of cells (col !stop) caret_lo in
  Printf.bprintf buf "%*s%s\n" snippet_indent "" shown;

  let pad = snippet_indent + caret_lo - offset in
  let hi = min (Sourcemap.rel ctx.sm (Span.hi span)) line_end in
  (* A span running off the window stops at its edge *)
  let width =
    if hi <= lo then 1 else min (col hi - caret_lo) (edge - caret_lo)
  in
  let markers = "^" ^ String.make (max 0 (width - 1)) '~' in
  Printf.bprintf buf "%*s%s" pad "" (colored ctx severity markers);
  Option.iter (Printf.bprintf buf " %s") label;
  Buffer.add_char buf '\n'

let render_with (context_at : int -> ctx) default_ctx d =
  let buf = Buffer.create 256 in
  let ctx =
    match d.primary with
    | Some span -> context_at (Span.lo span)
    | None -> default_ctx
  in
  Printf.bprintf buf "%s: %s\n" (severity_label ctx.color d.severity) d.headline;

  (match d.primary with
  | Some span -> render_snippet ctx buf span d.primary_label d.severity
  | None -> ());

  List.iter
    (fun label ->
      let label_ctx = context_at (Span.lo label.span) in
      render_snippet label_ctx buf label.span (Some label.message) Note)
    (List.rev d.labels);

  Option.iter (Buffer.add_string buf) d.detail;

  Option.iter
    (Printf.bprintf buf "%s: %s\n" (severity_label default_ctx.color Help))
    d.suggestion;
  Buffer.contents buf

let render ctx d = render_with (fun _ -> ctx) ctx d
