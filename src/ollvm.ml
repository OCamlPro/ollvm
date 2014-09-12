module Ast = Ast
module Printer = Printer
module Ez = Ez

let parse lexbuf =
  let parsing_err lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    let msg =
      Printf.sprintf "Parsing error: line %d, column %d, character '%s'"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        (Lexing.lexeme lexbuf)
    in failwith msg
  in
  try Parser.toplevelentries Lexer.token lexbuf
  with Parser.Error -> parsing_err lexbuf
