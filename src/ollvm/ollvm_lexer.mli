(*
exception Lex_error_unterminated_string of Lexing.position
val kw : string -> Ollvm_parser.token
type ident_type = Named | NamedString | Unnamed
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Ollvm_parser.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Ollvm_parser.token
val comment : Lexing.lexbuf -> Ollvm_parser.token
val __ocaml_lex_comment_rec : Lexing.lexbuf -> int -> Ollvm_parser.token
val string : Buffer.t -> Lexing.lexbuf -> string
val __ocaml_lex_string_rec : Buffer.t -> Lexing.lexbuf -> int -> string
val ident_body : Lexing.lexbuf -> string
val __ocaml_lex_ident_body_rec : Lexing.lexbuf -> int -> string
*)
val parse : Lexing.lexbuf -> Ollvm_ast.toplevelentries
