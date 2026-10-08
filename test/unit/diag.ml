(* SPDX-License-Identifier: Apache-2.0 *)

let ctx ?(color = false) src =
  {
    Ripe.Diagnostic.sm = Ripe.Sourcemap.create ~base:0 src;
    filename = "<test>";
    color;
  }

let render ?color src d =
  print_string (Ripe.Diagnostic.render (ctx ?color src) d)

let finish (value : 'a) : 'a * Ripe.Diagnostic.t list =
  let failed = Ripe.Diagnostic.has_errors () in
  let all = Ripe.Diagnostic.take () in
  if failed then raise (Ripe.Diagnostic.Errors all);
  (value, all)

(* This clears out whatever a crashed test left behind *)
let fresh () = ignore (Ripe.Diagnostic.take ())

let run_stage (f : unit -> 'a) : 'a * Ripe.Diagnostic.t list =
  fresh ();
  finish (f ())
