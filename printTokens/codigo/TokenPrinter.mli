open TokenDeclaration

val printTokens:
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
