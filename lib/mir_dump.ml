(* SPDX-License-Identifier: GPL-2.0-only *)

let storage_name (storage : Mir.storage_kind) : string =
  let open Mir in
  match storage with
  | Param -> "param"
  | User -> "user"
  | Temp -> "temp"
  | Result -> "result"

let constant (constant : Mir.constant) : string =
  let open Mir in
  match constant with
  | Int value -> Int64.to_string value
  | Float value -> Printf.sprintf "%.17g" value
  | Bool value -> string_of_bool value
  | Null -> "null"
  | CStr value -> Printf.sprintf "%S" value
  | Char value -> Printf.sprintf "U+%04X" value
  | Zero -> "zero"
  | Undef -> "undef"
  | Function name -> "@" ^ name
  | Str value -> Printf.sprintf "str %S" value

let unop_sym (op : Mir.unop) : string =
  let open Mir in
  match op with Neg -> "-" | Not -> "!" | BitNot -> "~"

let binop_sym (op : Mir.binop) : string =
  let open Mir in
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | Lshift -> "<<"
  | Rshift -> ">>"

let rec place (value : Mir.place) : string =
  let open Mir in
  let projection = function
    | Deref -> ".deref"
    | Field field -> Printf.sprintf ".field%d" field
    | Index index -> Printf.sprintf "[%s]" (operand index)
  in
  let base =
    match value.base with
    | Local id -> Printf.sprintf "%%%d" id
    | Global name -> "@" ^ name
  in
  Printf.sprintf "%s%s" base
    (String.concat "" (List.map projection value.projections))

and operand (value : Mir.operand) : string =
  let open Mir in
  match value.desc with
  | Copy source -> "copy " ^ place source
  | Const value -> constant value

let value (value : Mir.value) : string =
  let open Mir in
  match value.desc with
  | Use operand_value -> operand operand_value
  | Unary (op, operand_value) ->
      Printf.sprintf "%s%s" (unop_sym op) (operand operand_value)
  | Binary (op, left, right) ->
      Printf.sprintf "%s %s %s" (operand left) (binop_sym op) (operand right)
  | Cast (operand_value, kind) ->
      let checked =
        match kind with Ast.Checked -> "checked " | Ast.Normal -> ""
      in
      Printf.sprintf "%scast %s to %s" checked (operand operand_value)
        (Types.show_ty value.ty)
  | AddressOf source -> "address_of " ^ place source
  | Len source -> "len " ^ place source
  | DataPtr source -> "data_ptr " ^ place source
  | SizeOf ty -> "sizeof " ^ Types.show_ty ty

let callee (callee : Mir.callee) : string =
  let open Mir in
  match callee with
  | Direct name -> "@" ^ name
  | Indirect value -> operand value

let statement (statement : Mir.statement) : string =
  let open Mir in
  match statement.desc with
  | Assign (destination, assigned) ->
      Printf.sprintf "%s = %s" (place destination) (value assigned)
  | Call call ->
      let destination =
        match call.destination with
        | None -> ""
        | Some value -> place value ^ " = "
      in
      Printf.sprintf "%scall %s(%s)" destination (callee call.callee)
        (String.concat ", " (List.map operand call.args))
  | Slice (destination, source, lo, hi) ->
      Printf.sprintf "%s = slice %s %s %s" (place destination) (place source)
        (operand lo) (operand hi)

let check (check : Mir.check) : string =
  let open Mir in
  match check with
  | Bounds (index, length) ->
      Printf.sprintf "bounds %s %s" (operand index) (operand length)
  | SliceBounds (lo, hi, length) ->
      Printf.sprintf "slice_bounds %s %s %s" (operand lo) (operand hi)
        (operand length)
  | Null pointer -> Printf.sprintf "null %s" (operand pointer)
  | DivZero divisor -> Printf.sprintf "div_zero %s" (operand divisor)
  | NegativeShift count -> Printf.sprintf "negative_shift %s" (operand count)
  | CastRange (source, target) ->
      Printf.sprintf "cast %s %s" (operand source) (Types.show_ty target)

let terminator (value : Mir.terminator option) : string =
  let open Mir in
  match value with
  | None -> "<missing terminator>"
  | Some terminator -> (
      match terminator.desc with
      | Jump target -> Printf.sprintf "jump block%d" target
      | Branch (condition, yes, no) ->
          Printf.sprintf "branch %s block%d block%d" (operand condition) yes no
      | Assert (assertion, ok, fail) ->
          Printf.sprintf "assert_%s block%d block%d" (check assertion) ok fail
      | Panic failed -> "panic " ^ check failed
      | ReturnValue None -> "return"
      | ReturnValue (Some value) -> "return " ^ operand value
      | Unreachable -> "unreachable")

let func (func : Mir.func) : string =
  let open Mir in
  let buffer = Buffer.create 256 in
  let params =
    func.params
    |> List.map (fun id ->
        Printf.sprintf "%%%d: %s" id (Types.show_ty func.locals.(id).ty))
    |> String.concat ", "
  in
  let return_type =
    match func.return_ty with Types.TVoid -> "" | ty -> " " ^ Types.show_ty ty
  in
  Printf.bprintf buffer "func %s(%s)%s {\n" func.name params return_type;
  Array.iteri
    (fun id (local : local) ->
      let name = match local.name with None -> "" | Some name -> " " ^ name in
      Printf.bprintf buffer "  local %%%d%s: %s %s\n" id name
        (Types.show_ty local.ty)
        (storage_name local.storage))
    func.locals;
  if Array.length func.locals > 0 then Buffer.add_char buffer '\n';
  Array.iteri
    (fun id (block : block) ->
      Printf.bprintf buffer "  block%d:\n" id;
      List.iter
        (fun item -> Printf.bprintf buffer "    %s\n" (statement item))
        block.statements;
      Printf.bprintf buffer "    %s\n" (terminator block.terminator);
      if id + 1 < Array.length func.blocks then Buffer.add_char buffer '\n')
    func.blocks;
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

let global (global : Mir.global) : string =
  Printf.sprintf "global %s: %s\n" global.Mir.name (Types.show_ty global.Mir.ty)

let program (program : Mir.program) : string =
  String.concat ""
    (List.map global program.Mir.globals
    @ (if program.Mir.globals = [] then [] else [ "\n" ])
    @ List.mapi
        (fun index function_ ->
          (if index = 0 then "" else "\n") ^ func function_)
        program.Mir.functions)
