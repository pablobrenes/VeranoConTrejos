open Token

val printTokens:
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string -> unit
