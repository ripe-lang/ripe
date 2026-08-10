(* SPDX-License-Identifier: GPL-2.0-only *)

let ctx src =
  {
    Ripe.Diagnostic.sm = Ripe.Source_map.create ~base:0 src;
    filename = "<test>";
    color = false;
  }

let render src d = print_string (Ripe.Diagnostic.render (ctx src) d)

(* Offsets are global so a diagnostic renders against whichever file it hit *)
let render_in (program : Ripe.Program.t) d =
  let source_at = Ripe.Program.source_at program in
  let ctx_at pos =
    let source = source_at pos in
    {
      Ripe.Diagnostic.sm = source.Ripe.Program.source_map;
      filename = "<test>";
      color = false;
    }
  in
  print_string (Ripe.Diagnostic.render_with ctx_at (ctx_at 0) d)

let finish (diags : Ripe.Diagnostic.sink) (value : 'a) :
    'a * Ripe.Diagnostic.t list =
  let failed = Ripe.Diagnostic.has_errors diags in
  let all = Ripe.Diagnostic.drain diags in
  if failed then raise (Ripe.Diagnostic.Errors all);
  (value, all)

let run_stage (f : Ripe.Diagnostic.sink -> 'a) : 'a * Ripe.Diagnostic.t list =
  let diags = Ripe.Diagnostic.sink () in
  finish diags (f diags)
