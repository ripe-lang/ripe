(* SPDX-License-Identifier: Apache-2.0 *)

let ctx color src =
  { Ripe.Diagnostic.sm = Ripe.Sourcemap.create src; filename = "<test>"; color }

let render src d = print_string (Ripe.Diagnostic.render (ctx false src) d)

let finish value =
  let failed = Ripe.Diagnostic.has_errors () in
  let all = Ripe.Diagnostic.take () in
  if failed then raise (Ripe.Diagnostic.Errors all);
  (value, all)

(* This clears out whatever a crashed test left behind *)
let fresh () = ignore (Ripe.Diagnostic.take ())

let run_stage f =
  fresh ();
  finish (f ())
