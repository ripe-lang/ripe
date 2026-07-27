(* SPDX-License-Identifier: GPL-2.0-only *)

let ctx src =
  {
    Ripe.Diagnostic.sm = Ripe.Source_map.create src;
    filename = "<test>";
    color = false;
  }

let render src d = print_string (Ripe.Diagnostic.render (ctx src) d)

let finish (diags : Ripe.Diagnostic.sink) (value : 'a) :
    'a * Ripe.Diagnostic.t list =
  let failed = Ripe.Diagnostic.has_errors diags in
  let all = Ripe.Diagnostic.drain diags in
  if failed then raise (Ripe.Diagnostic.Errors all);
  (value, all)

let run_stage (f : Ripe.Diagnostic.sink -> 'a) : 'a * Ripe.Diagnostic.t list =
  let diags = Ripe.Diagnostic.sink () in
  finish diags (f diags)

let expect_errors f =
  try
    f ();
    print_endline "<no error>"
  with Ripe.Diagnostic.Errors diags ->
    List.iter (fun d -> print_string (Ripe.Diagnostic.render (ctx "") d)) diags
