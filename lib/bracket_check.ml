(* SPDX-License-Identifier: GPL-2.0-only *)

open Tokens

type step = Done | Stray | Open | Other

let opener_repr = function
  | LPAREN -> "`(`"
  | LBRACKET -> "`[`"
  | LBRACE -> "`{`"
  | _ -> ""

let closer_of = function
  | LPAREN -> Some RPAREN
  | LBRACKET -> Some RBRACKET
  | LBRACE -> Some RBRACE
  | _ -> None

let closer_repr = function
  | RPAREN -> "`)`"
  | RBRACKET -> "`]`"
  | RBRACE -> "`}`"
  | _ -> ""

let is_closer = function RPAREN | RBRACKET | RBRACE -> true | _ -> false

let report_mismatch diags span tok open_span open_tok =
  let want = Option.get (closer_of open_tok) in
  Diagnostic.emit diags
    Diagnostic.(
      error (Printf.sprintf "expected %s" (closer_repr want))
      |> at span
      |> label (Printf.sprintf "found %s" (show_token tok))
      |> secondary open_span ("unclosed " ^ opener_repr open_tok))

let report_stray diags span tok =
  Diagnostic.emit diags
    Diagnostic.(
      error "unexpected closing delimiter"
      |> at span
      |> label (Printf.sprintf "found %s" (show_token tok)))

let report_unclosed diags (open_tok, open_span) =
  Diagnostic.emit diags
    Diagnostic.(error ("unclosed " ^ opener_repr open_tok) |> at open_span)

(* This checks the token against open delimiters from the inside out *)
let step diags stack tok span =
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
