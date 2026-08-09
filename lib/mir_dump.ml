open Mir

let storage_name = function Param -> "param" | User -> "user" | Temp -> "temp"

let constant = function
  | Int value -> Int64.to_string value
  | Float value -> Printf.sprintf "%.17g" value
  | Bool value -> string_of_bool value
  | Null -> "null"
  | CStr value -> Printf.sprintf "%S" value
  | Char value -> Printf.sprintf "U+%04X" value
  | Zero -> "zero"
  | Undef -> "undef"
  | Function name -> "@" ^ name

let rec place (value : place) =
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

and operand (value : operand) =
  match value.desc with
  | Copy source -> "copy " ^ place source
  | Const value -> constant value

let value (value : value) =
  match value.desc with
  | Use operand_value -> operand operand_value
  | Unary (op, operand_value) ->
      Printf.sprintf "%s%s" (Ast.show_unop_sym op) (operand operand_value)
  | Binary (op, left, right) ->
      Printf.sprintf "%s %s %s" (operand left) (Ast.show_binop_sym op)
        (operand right)
  | Cast (operand_value, kind) ->
      let checked =
        match kind with Ast.Checked -> "checked " | Ast.Normal -> ""
      in
      Printf.sprintf "%scast %s to %s" checked (operand operand_value)
        (Types.show_ty value.ty)
  | AddressOf source -> "address_of " ^ place source
  | Len source -> "len " ^ place source
  | DataPtr source -> "data_ptr " ^ place source
  | ToSlice source -> "to_slice " ^ place source
  | Slice (source, lo, hi) ->
      Printf.sprintf "slice %s %s %s" (place source) (operand lo) (operand hi)
  | SizeOf ty -> "sizeof " ^ Types.show_ty ty

let callee = function
  | Direct name -> "@" ^ name
  | Indirect value -> operand value

let statement (statement : statement) =
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
  | Check (Bounds (index, length)) ->
      Printf.sprintf "check_bounds %s %s" (operand index) (operand length)
  | Check (SliceBounds (lo, hi, length)) ->
      Printf.sprintf "check_slice_bounds %s %s %s" (operand lo) (operand hi)
        (operand length)
  | Check (Null pointer) -> Printf.sprintf "check_null %s" (operand pointer)
  | Check (DivZero divisor) ->
      Printf.sprintf "check_div_zero %s" (operand divisor)
  | Check (NegativeShift count) ->
      Printf.sprintf "check_negative_shift %s" (operand count)
  | Check (CastRange (source, target)) ->
      Printf.sprintf "check_cast %s %s" (operand source) (Types.show_ty target)

let terminator (value : terminator option) =
  match value with
  | None -> "<missing terminator>"
  | Some terminator -> (
      match terminator.desc with
      | Jump target -> Printf.sprintf "jump block%d" target
      | Branch (condition, yes, no) ->
          Printf.sprintf "branch %s block%d block%d" (operand condition) yes no
      | ReturnValue None -> "return"
      | ReturnValue (Some value) -> "return " ^ operand value
      | Unreachable -> "unreachable")

let func (func : func) =
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

let global (global : global) =
  Printf.sprintf "global %s: %s\n" global.name (Types.show_ty global.ty)

let program (program : program) =
  String.concat ""
    (List.map global program.globals
    @ (if program.globals = [] then [] else [ "\n" ])
    @ List.mapi
        (fun index function_ ->
          (if index = 0 then "" else "\n") ^ func function_)
        program.functions)
