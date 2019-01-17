(**
This program was originally written by Luis Leopoldo P�rez on March 13, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Lexical Analyzer for Caml-Triangle
Interface file                                    

@author Luis Leopoldo P�rez
@author Jose Antonio Alp�zar Aguilar
@author Pablo Josu� Brenes Jimenes
@author Luis Jos� Castillo Valverde
*)


open Token


(** Reads a token from the buffer and returns it *)
val scan_token: Lexing.lexbuf -> token
