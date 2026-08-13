(* SPDX-License-Identifier: GPL-2.0-only *)

(* These diagnostics came from ceramic *)

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
let error_at (span : Ast.span) (msg : string) : t = error msg |> at span

(* Where a pass dumps diagnostics and the edge drains it to render *)
type sink = t list ref

let sink () : sink = ref []
let emit (s : sink) (d : t) : unit = s := d :: !s
let emit_error_at (s : sink) span msg = emit s (error_at span msg)
let emit_warn_at (s : sink) span msg = emit s (warning msg |> at span)

let has_errors (s : sink) : bool =
  List.exists (fun (d : t) -> d.severity = Error) !s

(* Sorted into source order and ties keep emission order *)
let drain (s : sink) : t list =
  let pos d = match d.primary with Some sp -> Span.lo sp | None -> -1 in
  List.stable_sort (fun a b -> compare (pos a) (pos b)) (List.rev !s)

(* The next stage would report these all over again if the sink kept them *)
let take (s : sink) : t list =
  let all = drain s in
  s := [];
  all

(* Rendering *)

type ctx = { sm : Source_map.t; filename : string; color : bool }

let tab_width = 8

(* Fixed rather than read from the terminal so expect tests stay reproducible *)
let snippet_width = 100
let snippet_indent = 4
let ellipsis = "..."

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

(* A byte offset isn't a column once tabs and multibyte characters show up *)
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
  let line, _ = Source_map.lookup ctx.sm (Span.lo span) in
  let src = Source_map.src ctx.sm in
  let lo = Source_map.rel ctx.sm (Span.lo span) in
  let line_start, _ = Source_map.line_bounds ctx.sm (Span.lo span) in
  let col = visual_col src line_start lo + 1 in
  Buffer.add_string buf (Printf.sprintf "  at %s:%d:%d\n" ctx.filename line col)

(* One entry per visual column so a window can be cut without splitting a character *)
let cells_of_line src line_start line_end : string Dynarray.t =
  let out = Dynarray.create () in
  let i = ref line_start in
  while !i < line_end do
    if src.[!i] = '\t' then begin
      let stop = ((Dynarray.length out / tab_width) + 1) * tab_width in
      while Dynarray.length out < stop do
        Dynarray.add_last out " "
      done;
      incr i
    end
    else begin
      let start = !i in
      incr i;
      while !i < line_end && is_cont src.[!i] do
        incr i
      done;
      Dynarray.add_last out (String.sub src start (!i - start))
    end
  done;
  out

(* A long line still has to show its caret so the window slides to it *)
let window_of (cells : string Dynarray.t) caret_lo : string * int =
  let total = Dynarray.length cells in
  let budget = snippet_width - snippet_indent in
  let buf = Buffer.create budget in
  let add lo hi =
    for i = lo to hi - 1 do
      Buffer.add_string buf (Dynarray.get cells i)
    done
  in
  if total <= budget then begin
    add 0 total;
    (Buffer.contents buf, 0)
  end
  else begin
    let start = max 0 (min (caret_lo - (budget / 2)) (total - budget)) in
    let cut = String.length ellipsis in
    let left = start > 0 and right = start + budget < total in
    if left then Buffer.add_string buf ellipsis;
    add
      (start + if left then cut else 0)
      (start + budget - if right then cut else 0);
    if right then Buffer.add_string buf ellipsis;
    (Buffer.contents buf, start)
  end

(* Offsets here index into the raw source so they have to be file relative *)
let render_snippet ctx buf (span : Ast.span) label severity =
  let src = Source_map.src ctx.sm in
  let lo = Source_map.rel ctx.sm (Span.lo span) in
  let line_start, line_end = Source_map.line_bounds ctx.sm (Span.lo span) in
  let cells = cells_of_line src line_start line_end in
  let caret_lo = visual_col src line_start lo in
  let shown, offset = window_of cells caret_lo in
  Buffer.add_string buf (String.make snippet_indent ' ');
  Buffer.add_string buf shown;
  Buffer.add_char buf '\n';
  Buffer.add_string buf (String.make snippet_indent ' ');
  let pad = caret_lo - offset in
  Buffer.add_string buf (String.make pad ' ');
  let hi = min (Source_map.rel ctx.sm (Span.hi span)) line_end in
  let markers =
    if hi <= lo then "^"
    else
      let w = visual_col src line_start hi - caret_lo in
      let w = if w < 1 then 1 else w in
      (* A span running off the window stops at its edge *)
      let w = min w (Dynarray.length cells - offset - pad) in
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

let render_with (context_at : int -> ctx) (default_ctx : ctx) (d : t) : string =
  let buf = Buffer.create 256 in
  let render_one_with d =
    let ctx =
      match d.primary with
      | Some span -> context_at (Span.lo span)
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
        let label_ctx = context_at (Span.lo label.span) in
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

let type_mismatch (span : Ast.span) ~(expected : string) ~(found : string) : t =
  error "type mismatch" |> at span
  |> label (Printf.sprintf "expected %s, found %s" expected found)

let undefined_name (span : Ast.span) (kind : string) : t =
  error ("undefined " ^ kind) |> at span

let with_type (span : Ast.span) (msg : string) (ty : string) : t =
  error msg |> at span |> label ("on " ^ ty)

let redefinition (span : Ast.span) ~(prev : Ast.span) : t =
  error_at span "already defined" |> secondary prev "previous definition here"

let arity (span : Ast.span) ~(expected : string) ~(found : int) : t =
  error "wrong number of arguments"
  |> at span
  |> label (Printf.sprintf "%s, found %d" expected found)

let unsupported_abi (span : Ast.span) : t =
  error "unsupported ABI" |> at span |> label "this ABI is not supported here"

let int_out_of_range (span : Ast.span) ~(ty : string) : t =
  error "integer literal out of range"
  |> at span
  |> label ("does not fit in " ^ ty)

let bad_operand (span : Ast.span) ~(op : string) ~(ty : string) : t =
  error "invalid operand" |> at span
  |> label (Printf.sprintf "cannot apply `%s` to %s" op ty)

let opaque_operation (span : Ast.span) (action : string) : t =
  error (Printf.sprintf "cannot %s *opaque" action)
  |> at span
  |> help "cast to a typed pointer first"

let cannot_infer (span : Ast.span) : t =
  error "cannot infer type" |> at span
  |> help "write the type or give it a value"

let expected_expression (span : Ast.span) : t =
  error "expected expression" |> at span

let expected_type (span : Ast.span) : t = error "expected type" |> at span

let with_found (span : Ast.span) (msg : string) (found : string) : t =
  error msg |> at span |> label ("found " ^ found)

let internal ?(span : Ast.span option) (msg : string) : t =
  let d =
    error "internal compiler error"
    |> detail (msg ^ "\n")
    |> help
         "this is a bug in ripec, please report it at \
          https://github.com/ripe-lang/ripe/issues"
  in
  match span with Some sp -> at sp d | None -> d

let ice ?(span : Ast.span option) (msg : string) : 'a =
  raise (Errors [ internal ?span msg ])
