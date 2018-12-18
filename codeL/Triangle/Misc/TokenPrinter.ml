open Printf
open String
open Parser

let tokenToStr r = (
  match r with
    INTLITERAL(a)   -> a
  | CHARLITERAL(a)  -> a
  | IDENTIFIER(a)   -> a
  | OPERATOR(a)     -> a
  | ARRAY           -> "array"
  | BEGIN           -> "begin"
  | CONST           -> "const"
  | DO              -> "do"
  | ELSE            -> "else"
  | END             -> "end"
  | FUNC            -> "func"
  | IF              -> "if"
  | IN              -> "in"
  | LET             -> "let"
  | OF              -> "of"
  | PROC            -> "proc"
  | RECORD          -> "record"
  | THEN            -> "then"
  | TYPE            -> "type"
  | VAR             -> "var"
  | WHILE           -> "while"
  | DOT             -> "."
  | COLON           -> ":"
  | SEMICOLON       -> ";"
  | COMMA           -> ","
  | BECOMES         -> ":="
  | IS              -> "~"
  | LPAREN          -> "("
  | RPAREN          -> ")"
  | LBRACKET        -> "["
  | RBRACKET        -> "]"
  | LCURLY          -> "{"
  | RCURLY          -> "}"
  | EOF             -> "end of file")

let rec printTokensAux scanner lexBuf chan = 
  let currentToken = scanner lexBuf in
  let tokenString = tokenToStr currentToken in
    output_string chan tokenString;
    output_string chan "|";

  if (currentToken <> EOF) then
    printTokensAux scanner lexBuf chan

let printTokens scanner lexBuf fileName =
  try
    let chan = open_out fileName in
      printTokensAux scanner lexBuf chan;
      close_out_noerr chan
  with Sys_error s -> printf "Couldn't write tokens file. (%s)\n" s