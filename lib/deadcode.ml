(* SPDX-License-Identifier: Apache-2.0 *)

open Mir

(* A C function stays because code outside ripe can call it by name *)
let is_root f = f.entry_point || (f.public && f.abi = Types.C)
let constant_refs add = function Function name -> add name | _ -> ()

let rec operand_refs add { desc; ty = _; span = _ } =
  match desc with Copy p -> place_refs add p | Const c -> constant_refs add c

and place_refs add p =
  (match p.base with Global name -> add name | Local _ -> ());
  List.iter
    (function Index o -> operand_refs add o | Deref | Field _ -> ())
    p.projections

let value_refs add = function
  | Use o | Unary (_, o) | Cast o -> operand_refs add o
  | Binary (_, a, b) ->
      operand_refs add a;
      operand_refs add b
  | AddressOf p | Len p | DataPtr p -> place_refs add p
  | SizeOf _ -> ()

let check_refs add = function
  | Bounds (a, b) ->
      operand_refs add a;
      operand_refs add b
  | SliceBounds (a, b, c) ->
      operand_refs add a;
      operand_refs add b;
      operand_refs add c
  | Null o | DivZero o | NegativeShift o -> operand_refs add o

let statement_refs add (s : statement) =
  match s.desc with
  | Assign (p, v) ->
      place_refs add p;
      value_refs add v.desc
  | Call c ->
      Option.iter (place_refs add) c.destination;
      (match c.callee with
      | Direct name -> add name
      | Indirect o -> operand_refs add o);
      List.iter (operand_refs add) c.args
  | Slice (dst, src, lo, hi) ->
      place_refs add dst;
      place_refs add src;
      operand_refs add lo;
      operand_refs add hi

let terminator_refs add (t : terminator) =
  match t.desc with
  | Branch (o, _, _) | ReturnValue (Some o) -> operand_refs add o
  | Assert (c, _, _) | Panic c -> check_refs add c
  | Jump _ | ReturnValue None | Unreachable -> ()

let func_refs add f =
  Array.iter
    (fun b ->
      List.iter (statement_refs add) b.statements;
      Option.iter (terminator_refs add) b.terminator)
    f.blocks

let rec global_refs add = function
  | GlobalConst (c, _) -> constant_refs add c
  | GlobalAddress name -> add name
  | GlobalArray vs -> List.iter (global_refs add) vs
  | GlobalStruct fields -> List.iter (fun (_, v) -> global_refs add v) fields

(* The rest was already checked so only code the program can reach is built *)
let strip program =
  let live = Hashtbl.create 64 and pending = Stack.create () in
  let add name =
    if not (Hashtbl.mem live name) then begin
      Hashtbl.replace live name ();
      Stack.push name pending
    end
  in

  let funcs = Hashtbl.create 64 and inits = Hashtbl.create 16 in
  List.iter
    (fun f ->
      if is_root f then add f.name;
      Hashtbl.replace funcs f.name f)
    program.functions;
  List.iter
    (fun g ->
      match g.init with
      | Some init -> Hashtbl.replace inits g.name init
      | None -> ())
    program.globals;

  while not (Stack.is_empty pending) do
    let name = Stack.pop pending in
    Option.iter (func_refs add) (Hashtbl.find_opt funcs name);
    Option.iter (global_refs add) (Hashtbl.find_opt inits name)
  done;
  {
    program with
    functions =
      List.filter
        (fun { name; blocks = _; _ } -> Hashtbl.mem live name)
        program.functions;
    globals =
      List.filter
        (fun { name; init = _; _ } -> Hashtbl.mem live name)
        program.globals;
  }
