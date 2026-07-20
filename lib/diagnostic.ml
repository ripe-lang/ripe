(* SPDX-License-Identifier: GPL-2.0-only *)

(* Compiler diagnostics ported from ceramic's diagnostic.{hpp,cpp} *)

type severity = Error | Warning | Note | Help
type span_label = { span : Ast.span; message : string }

type t = {
  severity : severity;
  headline : string;
  primary : Ast.span option; (* caret snippet target *)
  primary_label : string option; (* inline text after the caret *)
  labels : span_label list; (* secondary snippets *)
  notes : t list; (* "note:" sub diagnostics *)
  detail : string option; (* verbatim block after the snippet *)
  suggestion : string option; (* closing "help:" line *)
}

(* any pass raises this to abort with a batch of diagnostics *)
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

(* builder pipeline: `error msg |> at span |> label "..." |> help "..."` *)
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

(* where a pass dumps diagnostics and the edge drains it to render *)
type sink = t list ref

let sink () : sink = ref []
let emit (s : sink) (d : t) : unit = s := d :: !s
let error_at (s : sink) span msg = emit s (error msg |> at span)
let warn_at (s : sink) span msg = emit s (warning msg |> at span)

(* sorted into source order and ties keep emission order *)
let drain (s : sink) : t list =
  let pos d = match d.primary with Some sp -> sp.Ast.lo | None -> 0 in
  List.stable_sort (fun a b -> compare (pos a) (pos b)) (List.rev !s)

(* rendering *)

type ctx = { sm : Source_map.t; filename : string; color : bool }

let tab_width = 8

let severity_word = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"
  | Help -> "help"

let severity_ansi = function
  | Error -> "\027[1;31m"
  | Warning -> "\027[1;33m"
  | Note -> "\027[1;36m"
  | Help -> "\027[1;32m"

let reset = "\027[0m"
let colored ctx sev s = if ctx.color then severity_ansi sev ^ s ^ reset else s

let severity_label color sev =
  if color then severity_ansi sev ^ severity_word sev ^ reset
  else severity_word sev

(* utf8 continuation bytes do not advance a column *)
let is_cont c = Char.code c land 0xc0 = 0x80

(* visual columns from line_start to pos, tabs expanded, cont bytes skipped *)
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

(* headline, primary block, then each secondary label as its own block *)
let render_one ctx buf d =
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
    (fun (l : span_label) ->
      render_location ctx buf l.span;
      render_snippet ctx buf l.span (Some l.message) Note)
    d.labels

let render ctx d =
  let buf = Buffer.create 256 in
  render_one ctx buf d;
  (match d.detail with Some s -> Buffer.add_string buf s | None -> ());
  List.iter (render_one ctx buf) d.notes;
  (match d.suggestion with
  | Some s ->
      Buffer.add_string buf (colored ctx Help (severity_word Help));
      Buffer.add_string buf ": ";
      Buffer.add_string buf s;
      Buffer.add_char buf '\n'
  | None -> ());
  Buffer.contents buf
