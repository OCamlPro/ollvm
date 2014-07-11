let _ =
  Lexing.from_channel stdin
  |> Llvm_parser.module_ Llvm_lexer.token
  |> Printer.pprint
  |> print_string
