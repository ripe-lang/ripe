open Types
open Mir

type error = { function_name : string; span : Ast.span; message : string }

exception Invalid of error list

let show_error e = Printf.sprintf "%s: %s" e.function_name e.message

let verify (program : program) : unit =
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
  let add (func : func) span message =
    errors := { function_name = func.name; span; message } :: !errors
  in
  let verify_func (func : func) =
    Array.iter
      (fun (local : local) ->
        if local.ty = TError then add func local.span "local has no type")
      func.locals;
    let local (func : func) span id =
      if id < 0 || id >= Array.length func.locals then begin
        add func span (Printf.sprintf "local %d does not exist" id);
        None
      end
      else Some func.locals.(id).ty
    in
    let rec operand (func : func) (operand : operand) =
      match operand.desc with
      | Const _ -> Some operand.ty
      | Copy place_value ->
          let place_ty = place func place_value in
          (match place_ty with
          | Some ty when not (ty_equal ty operand.ty) ->
              add func operand.span
                (Printf.sprintf "operand has type %s but place has type %s"
                   (show_ty operand.ty) (show_ty ty))
          | Some _ | None -> ());
          place_ty
    and place (func : func) (place : place) =
      let rec project span ty = function
        | [] -> Some ty
        | Deref :: rest -> (
            match resolve_ty ty with
            | TPointer inner -> project span inner rest
            | _ ->
                add func span "deref projection requires a pointer";
                None)
        | Field field :: rest -> (
            match resolve_ty ty with
            | TStruct (name, _) -> (
                match Hashtbl.find_opt structs (Qname.key name) with
                | Some fields when field >= 0 && field < Array.length fields ->
                    project span fields.(field) rest
                | Some _ ->
                    add func span
                      (Printf.sprintf "field projection %d does not exist" field);
                    None
                | None ->
                    add func span
                      (Printf.sprintf "struct %s has no layout"
                         (Qname.show name));
                    None)
            | _ ->
                add func span "field projection requires a struct";
                None)
        | Index index :: rest -> (
            ignore (operand func index);
            if match resolve_ty index.ty with TInt _ -> false | _ -> true then
              add func index.span "index projection requires an integer";
            match resolve_ty ty with
            | TArray (inner, _) | TSlice inner | TPointer inner ->
                project span inner rest
            | _ ->
                add func span "index projection requires indexed storage";
                None)
      in
      let base_ty =
        match place.base with
        | Local id -> local func place.place_span id
        | Global name -> (
            match Hashtbl.find_opt globals name with
            | Some ty -> Some ty
            | None ->
                add func place.place_span
                  (Printf.sprintf "global %s does not exist" name);
                None)
      in
      match base_ty with
      | Some ty -> project place.place_span ty place.projections
      | None -> None
    in
    let value (func : func) (value : value) =
      let check_operand operand_value = ignore (operand func operand_value) in
      (match value.desc with
      | Use operand_value -> check_operand operand_value
      | Unary (_, operand_value) -> check_operand operand_value
      | Binary (_, left, right) ->
          check_operand left;
          check_operand right
      | Cast (operand_value, _) -> check_operand operand_value
      | AddressOf place_value
      | Len place_value
      | DataPtr place_value
      | ToSlice place_value ->
          ignore (place func place_value)
      | Slice (place_value, lo, hi) ->
          ignore (place func place_value);
          check_operand lo;
          check_operand hi
      | SizeOf _ -> ());
      value.ty
    in
    let statement (func : func) (statement : statement) =
      match statement.desc with
      | Assign (destination, assigned) -> (
          let destination_ty = place func destination in
          let assigned_ty = value func assigned in
          match destination_ty with
          | Some ty when not (Ty_pred.compatible ty assigned_ty) ->
              add func statement.span
                (Printf.sprintf "assignment stores %s in %s"
                   (show_ty assigned_ty) (show_ty ty))
          | Some _ | None -> ())
      | Check (Bounds (index, length)) ->
          ignore (operand func index);
          ignore (operand func length)
      | Check (SliceBounds (lo, hi, length)) ->
          ignore (operand func lo);
          ignore (operand func hi);
          ignore (operand func length)
      | Check (Null pointer) -> ignore (operand func pointer)
      | Check (DivZero divisor) -> ignore (operand func divisor)
      | Check (NegativeShift count) -> ignore (operand func count)
      | Check (CastRange (source, _)) -> ignore (operand func source)
      | Call call -> (
          (match call.callee with
          | Direct _ -> ()
          | Indirect callee -> ignore (operand func callee));
          List.iter (fun arg -> ignore (operand func arg)) call.args;
          let destination_ty = Option.bind call.destination (place func) in
          if is_aggregate call.return_ty then
            match destination_ty with
            | Some ty when ty_equal ty call.return_ty -> ()
            | Some ty ->
                add func statement.span
                  (Printf.sprintf
                     "aggregate result storage has type %s but call returns %s"
                     (show_ty ty) (show_ty call.return_ty))
            | None ->
                add func statement.span "aggregate call has no result storage"
          else
            match (call.return_ty, destination_ty) with
            | (TVoid | TNever), None -> ()
            | (TVoid | TNever), Some _ ->
                add func statement.span "void call has result storage"
            | return_ty, Some ty when ty_equal ty return_ty -> ()
            | return_ty, Some ty ->
                add func statement.span
                  (Printf.sprintf "call result has type %s but call returns %s"
                     (show_ty ty) (show_ty return_ty))
            | _, None -> add func statement.span "call has no result storage")
    in
    let block_exists span id =
      if id < 0 || id >= Array.length func.blocks then
        add func span (Printf.sprintf "block %d does not exist" id)
    in
    let terminator (terminator : terminator) =
      match terminator.desc with
      | Jump target -> block_exists terminator.span target
      | Branch (condition, yes, no) ->
          ignore (operand func condition);
          block_exists terminator.span yes;
          block_exists terminator.span no
      | ReturnValue returned -> (
          match (func.return_ty, returned) with
          | TVoid, None | TNever, None -> ()
          | TVoid, Some value ->
              ignore (operand func value);
              add func terminator.span "void function returns a value"
          | return_ty, Some value ->
              ignore (operand func value);
              if not (Ty_pred.compatible return_ty value.ty) then
                add func terminator.span
                  (Printf.sprintf "return has type %s but function returns %s"
                     (show_ty value.ty) (show_ty return_ty))
          | return_ty, None ->
              add func terminator.span
                (Printf.sprintf "return has no value for %s" (show_ty return_ty))
          )
      | Unreachable -> ()
    in
    List.iter
      (fun id -> match local func func.span id with Some _ -> () | None -> ())
      func.params;
    Array.iteri
      (fun id block ->
        List.iter (statement func) block.statements;
        match block.terminator with
        | Some term -> terminator term
        | None ->
            add func func.span (Printf.sprintf "block %d has no terminator" id))
      func.blocks
  in
  List.iter verify_func program.functions;
  match List.rev !errors with [] -> () | found -> raise (Invalid found)
