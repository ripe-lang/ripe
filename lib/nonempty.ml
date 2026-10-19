(* SPDX-License-Identifier: Apache-2.0 *)

(* This follows Stdune.Nonempty_list from https://github.com/ocaml/dune *)

type 'a t = ( :: ) of 'a * 'a list

let make (x : 'a) (xs : 'a list) : 'a t = x :: xs
let hd (x :: _ : 'a t) : 'a = x
let to_list (x :: xs : 'a t) : 'a list = Stdlib.List.cons x xs

let destruct_last (x :: xs : 'a t) : 'a list * 'a =
  match Stdlib.List.rev xs with
  | [] -> ([], x)
  | last :: rev_init -> (x :: Stdlib.List.rev rev_init, last)

let pp (f : Format.formatter -> 'a -> unit) fmt (t : 'a t) : unit =
  let sep fmt () = Format.fprintf fmt "; " in
  Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:sep f) (to_list t)
