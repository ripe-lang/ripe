let tokenize_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let rec loop () =
    let tok = Ripe.Lexer.read lexbuf in
    print_endline (Ripe.Token.to_string tok);
    if tok <> Ripe.Token.EOF then loop ()
  in
  loop ();
  close_in ic

let () =
  if Array.length Sys.argv < 2 then print_endline "Usage: ripe <file.rp>"
  else tokenize_file Sys.argv.(1)
