(* SPDX-License-Identifier: GPL-2.0-only *)

type error = { function_name : string; error_span : Ast.span; message : string }

exception Invalid of error list

let show_error (error : error) : string =
  Printf.sprintf "%s: %s" error.function_name error.message

(* TODO: Add a ctx record go group structs, globals, errors, and func *)
let verify (program : Mir.program) : unit =
  let open Types in
  let open Mir in
  let structs = Hashtbl.create 8 in
  List.iter
    (fun (decl : struct_decl) ->
      Hashtbl.replace structs (Qname.key decl.name) (Array.of_list decl.fields))
    program.structs;

  let globals = Hashtbl.create 8 in
  List.iter
    (fun (global : global) -> Hashtbl.replace globals global.name global.ty)
    program.globals;

  let errors = ref [] in

  (* func add(%0: i32, %1: i32) i32 { ... } *)
  let verify_func (func : func) =
    let add span message =
      errors :=
        { function_name = func.name; error_span = span; message } :: !errors
    in

    Array.iter
      (fun (local : local) ->
        if local.ty = TError then add local.span "local has no type")
      func.locals;

    (* local %0 value: i32 user *)
    let local span id =
      if id < 0 || id >= Array.length func.locals then begin
        add span (Printf.sprintf "local %d does not exist" id);
        None
      end
      else Some func.locals.(id).ty
    in

    (* copy %0 / 42 *)
    let rec operand (operand : operand) =
      match operand.desc with
      | Const _ -> Some operand.ty
      | Copy place_value ->
          let place_ty = place place_value in
          (match place_ty with
          | Some ty when not (ty_equal ty operand.ty) ->
              add operand.span
                (Printf.sprintf "operand has type %s but place has type %s"
                   (show_ty operand.ty) (show_ty ty))
          | Some _ | None -> ());
          place_ty
    (* %0 / @global / %0.deref.field0[copy %1] *)
    and place (place : place) =
      let span = place.place_span in
      let rec project ty = function
        | [] -> Some ty
        | Deref :: rest -> (
            match resolve_ty ty with
            | TPointer inner -> project inner rest
            | _ ->
                add span "deref projection requires a pointer";
                None)
        | Field field :: rest -> (
            match resolve_ty ty with
            | TStruct (name, _) -> (
                match Hashtbl.find_opt structs (Qname.key name) with
                | Some fields when field >= 0 && field < Array.length fields ->
                    project fields.(field) rest
                | Some _ ->
                    let message =
                      Printf.sprintf "field projection %d does not exist" field
                    in
                    add span message;
                    None
                | None ->
                    add span
                      (Printf.sprintf "struct %s has no layout"
                         (Qname.show name));
                    None)
            | _ ->
                add span "field projection requires a struct";
                None)
        | Index index :: rest -> (
            ignore (operand index);
            if match resolve_ty index.ty with TInt _ -> false | _ -> true then
              add index.span "index projection requires an integer";
            match resolve_ty ty with
            | TArray (inner, _) | TSlice inner | TPointer inner ->
                project inner rest
            | _ ->
                add span "index projection requires indexed storage";
                None)
      in
      let base_ty =
        match place.base with
        | Local id -> local span id
        | Global name -> (
            match Hashtbl.find_opt globals name with
            | Some ty -> Some ty
            | None ->
                add span (Printf.sprintf "global %s does not exist" name);
                None)
      in
      match base_ty with
      | Some ty -> project ty place.projections
      | None -> None
    in

    (* copy %0 / copy %0 + 1 / address_of %0 / len %0 *)
    let value (value : value) =
      let check_operand operand_value = ignore (operand operand_value) in

      (match value.desc with
      | Use operand_value -> check_operand operand_value
      | Unary (_, operand_value) -> check_operand operand_value
      | Binary (_, left, right) ->
          check_operand left;
          check_operand right
      | Cast (operand_value, _) -> check_operand operand_value
      | AddressOf place_value | Len place_value | DataPtr place_value ->
          ignore (place place_value)
      | SizeOf _ -> ());
      value.ty
    in

    (* bounds copy %0 copy %1 / null copy %0 *)
    let check (check : check) =
      let check_operand operand_value = ignore (operand operand_value) in
      match check with
      | Bounds (index, length) ->
          check_operand index;
          check_operand length
      | SliceBounds (lo, hi, length) ->
          check_operand lo;
          check_operand hi;
          check_operand length
      | Null pointer -> check_operand pointer
      | DivZero divisor -> check_operand divisor
      | NegativeShift count -> check_operand count
      | CastRange (source, _) -> check_operand source
    in

    (* %0 = copy %1 / %0 = call @add(copy %1) *)
    let statement (statement : statement) =
      let span = statement.span in
      match statement.desc with
      | Assign (destination, assigned) -> (
          let destination_ty = place destination in
          let assigned_ty = value assigned in
          match destination_ty with
          | Some ty when not (Ty_pred.compatible ty assigned_ty) ->
              add span
                (Printf.sprintf "assignment stores %s in %s"
                   (show_ty assigned_ty) (show_ty ty))
          | Some _ | None -> ())
      | ToSlice (destination, source) ->
          ignore (place destination);
          ignore (place source)
      | Slice (destination, source, lo, hi) ->
          ignore (place destination);
          ignore (place source);
          ignore (operand lo);
          ignore (operand hi)
      | Call call -> (
          (match call.callee with
          | Direct _ -> ()
          | Indirect callee -> ignore (operand callee));
          List.iter (fun arg -> ignore (operand arg)) call.args;
          let destination_ty = Option.bind call.destination place in
          if is_aggregate call.return_ty then
            match destination_ty with
            | Some ty when ty_equal ty call.return_ty -> ()
            | Some ty ->
                add span
                  (Printf.sprintf
                     "aggregate result storage has type %s but call returns %s"
                     (show_ty ty) (show_ty call.return_ty))
            | None -> add span "aggregate call has no result storage"
          else
            match (call.return_ty, destination_ty) with
            | (TVoid | TNever), None -> ()
            | (TVoid | TNever), Some _ ->
                add span "void call has result storage"
            | return_ty, Some ty when ty_equal ty return_ty -> ()
            | return_ty, Some ty ->
                add span
                  (Printf.sprintf "call result has type %s but call returns %s"
                     (show_ty ty) (show_ty return_ty))
            | _, None -> add span "call has no result storage")
    in

    (* block0 *)
    let block_exists span id =
      if id < 0 || id >= Array.length func.blocks then
        add span (Printf.sprintf "block %d does not exist" id)
    in

    (* jump block1 / branch %0 block1 block2 / return %0 / unreachable *)
    let terminator (terminator : terminator) =
      let span = terminator.span in
      match terminator.desc with
      | Jump target -> block_exists span target
      | Branch (condition, yes, no) ->
          ignore (operand condition);
          block_exists span yes;
          block_exists span no
      | Assert (assertion, ok, fail) ->
          check assertion;
          block_exists span ok;
          block_exists span fail
      | Panic failed -> check failed
      | ReturnValue (Some value) when func.result <> None ->
          ignore (operand value);
          add span "return has a value but the result is storage"
      | ReturnValue None when func.result <> None -> ()
      | ReturnValue returned -> (
          match (func.return_ty, returned) with
          | TVoid, None | TNever, None -> ()
          | TVoid, Some value ->
              ignore (operand value);
              add span "void function returns a value"
          | return_ty, Some value ->
              ignore (operand value);
              if not (Ty_pred.compatible return_ty value.ty) then
                add span
                  (Printf.sprintf "return has type %s but function returns %s"
                     (show_ty value.ty) (show_ty return_ty))
          | return_ty, None ->
              let message =
                Printf.sprintf "return has no value for %s" (show_ty return_ty)
              in
              add span message)
      | Unreachable -> ()
    in

    List.iter
      (fun id -> match local func.span id with Some _ -> () | None -> ())
      func.params;

    Array.iteri
      (fun id block ->
        List.iter statement block.statements;
        match block.terminator with
        | Some term -> terminator term
        | None -> add func.span (Printf.sprintf "block %d has no terminator" id))
      func.blocks
  in

  List.iter verify_func program.functions;

  match List.rev !errors with [] -> () | found -> raise (Invalid found)
