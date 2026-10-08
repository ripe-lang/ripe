(* SPDX-License-Identifier: Apache-2.0 *)

(* A test below the resolver has no real symbols so these stand in *)

open Ripe

let symbol ?(module_id = 0) ?(kind = Symbol.Type) ?(entry_point = false) ?name
    id =
  let name = Option.value name ~default:("s" ^ string_of_int id) in
  {
    Symbol.id;
    module_id;
    name;
    link_name = name;
    kind;
    entry_point;
    span = Span.dummy;
    name_span = Span.dummy;
  }

let key ?module_id ?kind id = Symbol.key (symbol ?module_id ?kind id)
let qname ?module_id id name = Qname.make (key ?module_id id) name
let struct_ty ?module_id id name = Types.TStruct (qname ?module_id id name, [])

let alias_ty ?module_id id name base =
  Types.TAlias (qname ?module_id id name, base)

let pred name f t = Printf.printf "%s %s = %b\n" name (Types.show_ty t) (f t)

let pred2 name f a b =
  Printf.printf "%s %s %s = %b\n" (Types.show_ty a) name (Types.show_ty b)
    (f a b)
