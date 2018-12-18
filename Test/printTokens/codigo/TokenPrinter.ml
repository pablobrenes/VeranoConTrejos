open Printf
open String
open TokenDeclaration

let tokenToStr r = (
  match r with
    INTLITERAL(a)   -> "integer literal"
  | CHARLITERAL(a)  -> "character literal"
  | IDENTIFIER(a)   -> "identifier"
  | OPERATOR(a)     -> "operator"
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

let rec printTokens scanner lexBuf = 
  let currentToken = scanner lexBuf in
  let tokenString = tokenToStr currentToken in
  print_string tokenString;
  printf "|";

  if (currentToken <> EOF) then
    printTokens scanner lexBuf;