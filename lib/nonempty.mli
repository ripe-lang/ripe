(* SPDX-License-Identifier: Apache-2.0 *)

(* A list that always has something in it so no caller needs a dead branch *)
type 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val make : 'a -> 'a list -> 'a t
val hd : 'a t -> 'a
val destruct_last : 'a t -> 'a list * 'a
val to_list : 'a t -> 'a list
