open Parser

val printTokens:
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string -> unit
