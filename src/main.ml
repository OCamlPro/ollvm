let parsing_err lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let msg =
    Printf.sprintf "Parsing error: line %d, column %d, character '%s'"
                   pos.Lexing.pos_lnum
                   (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
                   (Lexing.lexeme lexbuf)
  in failwith msg

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    lexbuf
    |> Llvm_parser.module_ Llvm_lexer.token
    |> Printer.pprint
    |> print_string
  with Llvm_parser.Error -> parsing_err lexbuf
