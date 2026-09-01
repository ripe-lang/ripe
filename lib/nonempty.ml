(* SPDX-License-Identifier: Apache-2.0 *)

(* This follows Stdune.Nonempty_list from https://github.com/ocaml/dune *)

type 'a t = ( :: ) of 'a * 'a list

let make (x : 'a) (xs : 'a list) : 'a t = x :: xs
let one (x : 'a) : 'a t = x :: []
let hd (x :: _ : 'a t) : 'a = x
let to_list (x :: xs : 'a t) : 'a list = Stdlib.List.cons x xs
let cons (x : 'a) (t : 'a t) : 'a t = x :: to_list t
let map_hd (f : 'a -> 'a) (x :: xs : 'a t) : 'a t = f x :: xs

let find_map (f : 'a -> 'b option) (x :: xs : 'a t) : 'b option =
  match f x with Some _ as found -> found | None -> Stdlib.List.find_map f xs

let destruct_last (x :: xs : 'a t) : 'a list * 'a =
  match Stdlib.List.rev xs with
  | [] -> ([], x)
  | last :: rev_init -> (x :: Stdlib.List.rev rev_init, last)

let pp (f : Format.formatter -> 'a -> unit) fmt (t : 'a t) : unit =
  let sep fmt () = Format.fprintf fmt "; " in
  Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:sep f) (to_list t)
