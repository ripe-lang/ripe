(* SPDX-License-Identifier: Apache-2.0 *)

let tok_str t =
  let open Ripe.Tokens in
  match t with
  | IDENT s -> "IDENT " ^ s
  | INT (n, suf) -> "INT " ^ Int64.to_string n ^ Option.value ~default:"" suf
  | FLOAT (f, suf) ->
      "FLOAT " ^ string_of_float f ^ Option.value ~default:"" suf
  | STRING s -> "STRING " ^ String.escaped s
  | SEMI -> "SEMI"
  | EOF -> "EOF"
  | ERROR _ -> "ERROR"
  | other ->
      if List.exists (fun (_, t') -> t' = other) keywords then
        "KW " ^ show_token other
      else show_token other

let dump_ident i = Ripe.Interner.text i.Ripe.Ast.value

(* A compact s-expr keeps expr trees free of spans *)
let rec dump_expr e =
  match e.Ripe.Ast.edesc with
  | Int (n, suf) -> Int64.to_string n ^ Option.value ~default:"" suf
  | Float (f, suf) -> string_of_float f ^ Option.value ~default:"" suf
  | Bool b -> string_of_bool b
  | Null -> "null"
  | Char c -> Ripe.Tokens.show_token (CHAR c)
  | String s -> "\"" ^ s ^ "\""
  | Ident s -> Ripe.Interner.text s
  | Call (callee, args) ->
      "(call " ^ dump_expr callee
      ^ String.concat "" (List.map (fun a -> " " ^ dump_expr a) args)
      ^ ")"
  | BinOp (op, l, r) ->
      "("
      ^ Ripe.Ast.show_binop_sym op.value
      ^ " " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | Assign (base, l, r) ->
      let sym =
        match base.value with
        | None -> "="
        | Some op -> Ripe.Ast.show_binop_sym op ^ "="
      in
      "(" ^ sym ^ " " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | UnOp (op, e) -> "(" ^ Ripe.Ast.show_unop_sym op ^ " " ^ dump_expr e ^ ")"
  | Range (l, r) -> "(.. " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | RangeInclusive (l, r) -> "(..= " ^ dump_expr l ^ " " ^ dump_expr r ^ ")"
  | RangeFrom l -> "(.. " ^ dump_expr l ^ ")"
  | RangeTo r -> "(.. " ^ dump_expr r ^ ")"
  | RangeToInclusive r -> "(..= " ^ dump_expr r ^ ")"
  | RangeFull -> "(..)"
  | Path p ->
      let path, member = Ripe.Ast.path_split p in
      "(. "
      ^ String.concat " " (List.map Ripe.Interner.text (path @ [ member ]))
      ^ ")"
  | FieldAccess (e, f) ->
      "(. " ^ dump_expr e ^ " " ^ Ripe.Interner.text f.value ^ ")"
  | Cast (t, e) -> "(cast " ^ dump_expr t ^ " " ^ dump_expr e ^ ")"
  | SizeOf t -> "(sizeof " ^ dump_expr t ^ ")"
  | ArrayLit elems ->
      "(array"
      ^ String.concat "" (List.map (fun e -> " " ^ dump_expr e) elems)
      ^ ")"
  | Index (base, idx) -> "(index " ^ dump_expr base ^ " " ^ dump_expr idx ^ ")"
  | StructLit (name, fields) ->
      "(struct "
      ^ Option.fold ~none:"."
          ~some:(fun n -> Ripe.Interner.text n.Ripe.Ast.value)
          name
      ^ String.concat ""
          (List.map
             (fun (f, e) ->
               match f.Ripe.Ast.value with
               | Some f -> " (" ^ Ripe.Interner.text f ^ " " ^ dump_expr e ^ ")"
               | None -> " " ^ dump_expr e)
             fields)
      ^ ")"
  | DotVariant name -> "(. " ^ Ripe.Interner.text name.value ^ ")"
  | Block body -> dump_block body
  | If (branches, else_body) ->
      let arm (c, body) =
        "(" ^ dump_expr c ^ " " ^ dump_block body.Ripe.Ast.value ^ ")"
      in
      "(if "
      ^ String.concat " " (List.map arm branches)
      ^ (match else_body with
        | Some b -> " " ^ dump_block b.Ripe.Ast.value
        | None -> "")
      ^ ")"
  | Loop (_, _, body) -> "(loop " ^ dump_block body ^ ")"
  | For (_, name, iter, body) ->
      "(for "
      ^ Ripe.Interner.text name.value
      ^ " " ^ dump_expr iter ^ " " ^ dump_block body ^ ")"
  | Binding b ->
      let head =
        match b.binder with RunTime -> "(bind " | CompileTime -> "(:: "
      in
      head ^ dump_ident b.bind_name
      ^ (match Ripe.Ast.bind_value b with
        | Some e -> " " ^ dump_expr e
        | None -> "")
      ^ ")"
  | Return e ->
      "(return"
      ^ (match e with Some e -> " " ^ dump_expr e | None -> "")
      ^ ")"
  | Break (_, Some e) -> "(break " ^ dump_expr e ^ ")"
  | Break (_, None) -> "(break)"
  | Continue _ -> "(continue)"
  | Match (scrutinee, arms) ->
      let arm a =
        " ("
        ^ dump_pattern a.Ripe.Ast.pat
        ^ " "
        ^ dump_block a.arm_body.Ripe.Ast.value
        ^ ")"
      in
      "(match " ^ dump_expr scrutinee
      ^ String.concat "" (List.map arm arms)
      ^ ")"
  | Unit -> "()"
  | Lambda l -> "(fn " ^ dump_block l.lambda_body ^ ")"
  | StructType fields ->
      let field f =
        " ("
        ^ dump_ident f.Ripe.Ast.field_name
        ^ " " ^ dump_expr f.field_typ ^ ")"
      in
      "(struct_type" ^ String.concat "" (List.map field fields) ^ ")"
  | EnumType variants ->
      let variant v =
        match v.Ripe.Ast.variant_payload with
        | [] -> " " ^ dump_ident v.variant_name
        | payload ->
            " (" ^ dump_ident v.variant_name
            ^ String.concat "" (List.map (fun t -> " " ^ dump_expr t) payload)
            ^ ")"
      in
      "(enum_type" ^ String.concat "" (List.map variant variants) ^ ")"
  | PointerType t -> "*" ^ dump_expr t
  | ArrayType (n, t) -> "[" ^ dump_expr n ^ "]" ^ dump_expr t
  | SliceType t -> "[]" ^ dump_expr t
  | FuncType (_, params, ret) ->
      let ps = String.concat ", " (List.map dump_expr params) in
      "(" ^ ps ^ ") -> " ^ dump_expr ret
  | Hole -> "_"
  | Extern abi -> Printf.sprintf "extern %S" abi.value

and dump_pattern p =
  match p.Ripe.Ast.pdesc with
  | PatValue e -> dump_expr e
  | PatWild -> "_"
  | PatBind name -> Ripe.Interner.text name
  | PatVariant (callee, subs) ->
      "(" ^ dump_expr callee
      ^ String.concat "" (List.map (fun p -> " " ^ dump_pattern p) subs)
      ^ ")"

and dump_block body =
  "(block " ^ String.concat " " (List.map dump_expr body) ^ ")"

let dump_tokens src =
  Diag.fresh ();
  let lexbuf = Ripe.Lexer.lexbuf_of_string src in
  let rec go () =
    let t, _ = Ripe.Lexer.read lexbuf in
    print_endline (tok_str t);
    if t <> Ripe.Tokens.EOF then go ()
  in
  go ();
  List.iter
    (fun d -> print_endline ("error " ^ Ripe.Diagnostic.headline d))
    (Ripe.Diagnostic.take ())
