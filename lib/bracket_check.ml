(* SPDX-License-Identifier: GPL-2.0-only *)

open Tokens

type step = Done | Stray | Open | Other

let opener_repr (tok : token) : string =
  match tok with
  | LPAREN -> "`(`"
  | LBRACKET -> "`[`"
  | LBRACE -> "`{`"
  | _ -> ""

let closer_of (tok : token) : token option =
  match tok with
  | LPAREN -> Some RPAREN
  | LBRACKET -> Some RBRACKET
  | LBRACE -> Some RBRACE
  | _ -> None

let closer_repr (tok : token) : string =
  match tok with
  | RPAREN -> "`)`"
  | RBRACKET -> "`]`"
  | RBRACE -> "`}`"
  | _ -> ""

let is_closer (tok : token) : bool =
  match tok with RPAREN | RBRACKET | RBRACE -> true | _ -> false

let report_mismatch (diags : Diagnostic.sink) (span : Ast.span) (tok : token)
    (open_span : Ast.span) (open_tok : token) : unit =
  let want = Option.get (closer_of open_tok) in
  Diagnostic.emit diags
    Diagnostic.(
      error (Printf.sprintf "expected %s" (closer_repr want))
      |> at span
      |> label (Printf.sprintf "found %s" (show_token tok))
      |> secondary open_span ("unclosed " ^ opener_repr open_tok))

let report_stray (diags : Diagnostic.sink) (span : Ast.span) (tok : token) :
    unit =
  Diagnostic.emit diags
    Diagnostic.(
      error "unexpected closing delimiter"
      |> at span
      |> label (Printf.sprintf "found %s" (show_token tok)))

let report_unclosed (diags : Diagnostic.sink) (open_tok, open_span) : unit =
  Diagnostic.emit diags
    Diagnostic.(error ("unclosed " ^ opener_repr open_tok) |> at open_span)

(* This checks the token against open delimiters from the inside out *)
let step (diags : Diagnostic.sink) (stack : (token * Ast.span) list)
    (tok : token) (span : Ast.span) : step =
  if is_closer tok then (
    match stack with
    | (open_tok, _) :: _ when closer_of open_tok = Some tok -> Done
    | (open_tok, open_span) :: _ ->
        report_mismatch diags span tok open_span open_tok;
        (* This mismatch throws everything off so stop before the cascade *)
        raise (Diagnostic.Errors (Diagnostic.drain diags))
    | [] ->
        report_stray diags span tok;
        Stray)
  else if tok = EOF then
    if stack <> [] then begin
      (* The stack is reversed so errors show up in source order *)
      List.iter (report_unclosed diags) (List.rev stack);
      raise (Diagnostic.Errors (Diagnostic.drain diags))
    end
    else Done
  else if closer_of tok <> None then Open
  else Other
