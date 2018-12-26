open Token

val parseProgram :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.astProgram
