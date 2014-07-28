module IR = LLVM
module Printer = Printer


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
  try lexbuf
      |> Llvm_parser.module_ Llvm_lexer.token
      |> Printer.module_
      |> print_endline
  with Llvm_parser.Error -> parsing_err lexbuf
