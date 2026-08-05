(* SPDX-License-Identifier: GPL-2.0-only *)

(* These diagnostics came from ceramic *)

open Span

type severity = Error | Warning | Note | Help
type span_label = { span : Ast.span; message : string }

type t = {
  severity : severity;
  headline : string;
  primary : Ast.span option; (* Caret snippet target *)
  primary_label : string option; (* Inline text after the caret *)
  labels : span_label list; (* Secondary snippets *)
  notes : t list; (* "note:" sub diagnostics *)
  detail : string option; (* Verbatim block after the snippet *)
  suggestion : string option; (* Closing "help:" line *)
}

exception Errors of t list

let make severity headline =
  {
    severity;
    headline;
    primary = None;
    primary_label = None;
    labels = [];
    notes = [];
    detail = None;
    suggestion = None;
  }

(* Builder pipeline: `error msg |> at span |> label "..." |> help "..."` *)
let error headline = make Error headline
let warning headline = make Warning headline
let note headline = make Note headline
let at span d = { d with primary = Some span }
let label message d = { d with primary_label = Some message }

let secondary span message d =
  { d with labels = d.labels @ [ { span; message } ] }

let add_note n d = { d with notes = d.notes @ [ n ] }
let detail s d = { d with detail = Some s }
let help s d = { d with suggestion = Some s }

(* Where a pass dumps diagnostics and the edge drains it to render *)
type sink = t list ref

let sink () : sink = ref []
let emit (s : sink) (d : t) : unit = s := d :: !s
let error_at (s : sink) span msg = emit s (error msg |> at span)
let warn_at (s : sink) span msg = emit s (warning msg |> at span)

let has_errors (s : sink) : bool =
  List.exists (fun (d : t) -> d.severity = Error) !s

(* Sorted into source order and ties keep emission order *)
let drain (s : sink) : t list =
  let pos d =
    match d.primary with Some sp -> (sp.Ast.file, sp.lo) | None -> (-1, 0)
  in
  List.stable_sort (fun a b -> compare (pos a) (pos b)) (List.rev !s)

(* This clears the sink so the next stage doesn't report these again *)
let take (s : sink) : t list =
  let all = drain s in
  s := [];
  all

(* Rendering *)

type ctx = { sm : Source_map.t; filename : string; color : bool }

let tab_width = 8

let severity_word (severity : severity) =
  match severity with
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"
  | Help -> "help"

let severity_ansi (severity : severity) =
  match severity with
  | Error -> "\027[1;31m"
  | Warning -> "\027[1;33m"
  | Note -> "\027[1;36m"
  | Help -> "\027[1;32m"

let reset = "\027[0m"
let colored ctx sev s = if ctx.color then severity_ansi sev ^ s ^ reset else s

let severity_label color sev =
  if color then severity_ansi sev ^ severity_word sev ^ reset
  else severity_word sev

(* UTF8 continuation bytes don't advance a column *)
let is_cont c = Char.code c land 0xc0 = 0x80

(* This gets visual columns from line_start to pos with tabs expanded and cont bytes skipped *)
let visual_col src line_start pos =
  let col = ref 0 in
  for i = line_start to pos - 1 do
    let c = src.[i] in
    if c = '\t' then col := ((!col / tab_width) + 1) * tab_width
    else if is_cont c then ()
    else incr col
  done;
  !col

let render_location ctx buf (span : Ast.span) =
  let line, _ = Source_map.lookup ctx.sm span.lo in
  let src = ctx.sm.Source_map.src in
  let line_start, _ = Source_map.line_bounds ctx.sm span.lo in
  let col = visual_col src line_start span.lo + 1 in
  Buffer.add_string buf (Printf.sprintf "  at %s:%d:%d\n" ctx.filename line col)

let render_snippet ctx buf (span : Ast.span) label severity =
  let src = ctx.sm.Source_map.src in
  let line_start, line_end = Source_map.line_bounds ctx.sm span.lo in
  Buffer.add_string buf "    ";
  Buffer.add_substring buf src line_start (line_end - line_start);
  Buffer.add_char buf '\n';
  Buffer.add_string buf "    ";
  let pad = visual_col src line_start span.lo in
  Buffer.add_string buf (String.make pad ' ');
  let hi = min span.hi line_end in
  let markers =
    if hi <= span.lo then "^"
    else
      let w = visual_col src line_start hi - pad in
      let w = if w < 1 then 1 else w in
      "^" ^ String.make (w - 1) '~'
  in
  Buffer.add_string buf (colored ctx severity markers);
  (match label with
  | Some l ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf l
  | None -> ());
  Buffer.add_char buf '\n'

let render_with (context_for_file : Span.file_id -> ctx) (default_ctx : ctx)
    (d : t) : string =
  let buf = Buffer.create 256 in
  let render_one_with d =
    let ctx =
      match d.primary with
      | Some span -> context_for_file span.Ast.file
      | None -> default_ctx
    in
    Buffer.add_string buf (colored ctx d.severity (severity_word d.severity));
    Buffer.add_string buf ": ";
    Buffer.add_string buf d.headline;
    Buffer.add_char buf '\n';
    (match d.primary with
    | Some span ->
        render_location ctx buf span;
        render_snippet ctx buf span d.primary_label d.severity
    | None -> ());
    List.iter
      (fun (label : span_label) ->
        let label_ctx = context_for_file label.span.file in
        render_location label_ctx buf label.span;
        render_snippet label_ctx buf label.span (Some label.message) Note)
      d.labels
  in
  render_one_with d;
  (match d.detail with
  | Some detail -> Buffer.add_string buf detail
  | None -> ());
  List.iter render_one_with d.notes;
  (match d.suggestion with
  | Some suggestion ->
      Buffer.add_string buf (colored default_ctx Help (severity_word Help));
      Buffer.add_string buf ": ";
      Buffer.add_string buf suggestion;
      Buffer.add_char buf '\n'
  | None -> ());
  Buffer.contents buf

let render (ctx : ctx) (d : t) : string = render_with (fun _ -> ctx) ctx d
