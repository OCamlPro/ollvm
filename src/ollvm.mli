module Ast : module type of struct include Ast end
module Printer : module type of struct include Printer end
module Ez : module type of struct include Ez end

val parse : Lexing.lexbuf -> Ast.toplevelentries
