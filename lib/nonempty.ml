(* SPDX-License-Identifier: Apache-2.0 *)

type 'a t = ( :: ) of 'a * 'a list

let make x xs = x :: xs
let hd (x :: _) = x
let to_list (x :: xs) = Stdlib.List.cons x xs

let destruct_last =
  let rec loop acc = function
    | [ x ] -> (List.rev acc, x)
    | x :: y :: xs -> loop (x :: acc) (y :: xs)
  in
  fun xs -> loop [] xs

let pp f fmt t =
  let sep fmt () = Format.fprintf fmt "; " in
  Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:sep f) (to_list t)
