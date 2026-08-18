(* SPDX-License-Identifier: Apache-2.0 *)

type t = int

(* An OCaml int is 63 bits so the start and the length get 31 each and the sign
   bit stays clear which caps a program at two gigabytes of source *)
let offset_bits = 31
let offset_mask = (1 lsl offset_bits) - 1

(* The dummy span starts at -1 so the stored offset is biased by one *)
let max_offset = offset_mask - 1

(* Packing the start above the length keeps a span out of the heap and makes
   two of them compare on where they start *)
let make (lo : int) (hi : int) : t = ((lo + 1) lsl offset_bits) lor (hi - lo)
let lo (t : t) : int = (t lsr offset_bits) - 1
let hi (t : t) : int = lo t + (t land offset_mask)
let dummy : t = make (-1) (-1)

let pp (fmt : Format.formatter) (t : t) : unit =
  Format.fprintf fmt "(%d,%d)" (lo t) (hi t)

let show (t : t) : string = Format.asprintf "%a" pp t

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let equal (a : t) (b : t) : bool = a = b
  let hash (t : t) : int = t lsr offset_bits
end)
