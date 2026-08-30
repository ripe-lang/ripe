(* SPDX-License-Identifier: Apache-2.0 *)

open Tokens

type opener = Paren | Bracket | Brace
type opened = opener * Ast.span
type step = End | Closed of opened list | Stray | Open of opener | Other

let opener_repr = function Paren -> "`(`" | Bracket -> "`[`" | Brace -> "`{`"

let opener_of = function
  | LPAREN -> Some Paren
  | LBRACKET -> Some Bracket
  | LBRACE -> Some Brace
  | _ -> None

let closer_of = function
  | Paren -> RPAREN
  | Bracket -> RBRACKET
  | Brace -> RBRACE

let closer_repr = function
  | RPAREN -> "`)`"
  | RBRACKET -> "`]`"
  | RBRACE -> "`}`"
  | _ -> ""

let is_closer = function RPAREN | RBRACKET | RBRACE -> true | _ -> false

let report_mismatch diags span open_span opener =
  let want = closer_of opener in
  Diagnostic.emit diags
    Diagnostic.(
      error "mismatched delimiter"
      |> at span
      |> label (Printf.sprintf "expected %s" (closer_repr want))
      |> secondary open_span ("unclosed " ^ opener_repr opener))

let report_unclosed diags (_, open_span) =
  Diagnostic.emit diags Diagnostic.(error "unclosed delimiter" |> at open_span)

(* This checks the token against open delimiters from the inside out *)
let step diags stack tok span =
  if is_closer tok then
    match stack with
    | (opener, _) :: rest when closer_of opener = tok -> Closed rest
    | (opener, open_span) :: _ ->
        report_mismatch diags span open_span opener;
        (* This mismatch throws everything off so stop before the cascade *)
        raise (Diagnostic.Errors (Diagnostic.drain diags))
    | [] -> Stray
  else if tok = EOF then
    if not (List.is_empty stack) then begin
      (* The stack is reversed so errors show up in source order *)
      List.iter (report_unclosed diags) (List.rev stack);
      raise (Diagnostic.Errors (Diagnostic.drain diags))
    end
    else End
  else match opener_of tok with Some opener -> Open opener | None -> Other
