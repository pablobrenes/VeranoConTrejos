(* ------------------------------------------------------ *)
(* Tokens Drawing Library in a plain text file for        *)
(* Caml-Triangle                                          *)
(* Implementation file                                    *)
(*                                                        *)
(* Last modification by:                                  *)
(* Jose Antonio Alpizar Aguilar - 2016201868              *)
(* Pablo Josué Brenes Jiménez - 2016250460                *)
(* Luis José Castillo Valverde - 2016094804               *)
(* 22/12/2018                                             *)
(* ------------------------------------------------------ *)

open Printf
open String
open Token

(* This function given a token returns its equivalent in string. *)
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


(* This function receives the scanner function, the buffer of the file 
being scanned and the handler of the file in which it is being written. 
It is responsible for filling the file with the tokens separated by pipes. *)
let rec printTokensAux scanner lexBuf chan = 
  let currentToken = scanner lexBuf in
  let tokenString = tokenToStr currentToken in
    output_string chan tokenString;
    output_string chan "|";

  if (currentToken <> EOF) then
    printTokensAux scanner lexBuf chan


(* This function receives the function of scanner, the buffer of the file 
that is being scanned and the name of the file in which it will be written. 
Is responsible for creating the file. *)
let printTokens scanner lexBuf fileName =
  try
    let chan = open_out fileName in
      printTokensAux scanner lexBuf chan;
      close_out_noerr chan
  with Sys_error s -> printf "Couldn't write tokens file. (%s)\n" s