(* SPDX-License-Identifier: Apache-2.0 *)

(* A test below the resolver has no real symbols so these stand in *)

open Ripe

let symbol ?(kind = Symbol.Type) ?(entry_point = false) ?name id =
  let name = Option.value name ~default:("s" ^ string_of_int id) in
  {
    Symbol.id;
    name;
    link_name = name;
    kind;
    entry_point;
    span = Span.dummy;
    name_span = Span.dummy;
  }

let key ?kind id = Symbol.key (symbol ?kind id)
let keyname id name = Keyname.make (key id) name
let struct_ty id name = Types.TStruct (keyname id name, [])
let alias_ty id name base = Types.TAlias (keyname id name, base)
let pred name f t = Printf.printf "%s %s = %b\n" name (Types.show_ty t) (f t)

let pred2 name f a b =
  Printf.printf "%s %s %s = %b\n" (Types.show_ty a) name (Types.show_ty b)
    (f a b)
