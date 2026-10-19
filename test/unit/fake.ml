(* SPDX-License-Identifier: Apache-2.0 *)

(* A test below the resolver has no real symbols so these stand in *)

open Ripe

let symbol kind name id =
  {
    Symbol.id;
    name;
    link_name = name;
    kind;
    entry_point = false;
    span = Span.dummy;
    name_span = Span.dummy;
  }

let key id = Symbol.key (symbol Symbol.Type ("s" ^ string_of_int id) id)
let keyname id name = Keyname.make (key id) name
let struct_ty id name = Types.TAdt (keyname id name, [])
let alias_ty id name base = Types.TAlias (keyname id name, base)

let func_ty sig_params sig_ret sig_abi sig_arity =
  Types.TFunc { sig_params; sig_ret; sig_abi; sig_arity }

let hex s =
  String.to_seq s
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat " "

let pred name f t = Printf.printf "%s %s = %b\n" name (Types.show_ty t) (f t)

let pred2 name f a b =
  Printf.printf "%s %s %s = %b\n" (Types.show_ty a) name (Types.show_ty b)
    (f a b)
