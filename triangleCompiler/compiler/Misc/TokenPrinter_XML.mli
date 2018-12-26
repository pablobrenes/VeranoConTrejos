(* ------------------------------------------------------ *)
(* Tokens Drawing Library in a XML file for Caml-Triangle *)
(* Interface file                                         *)
(*                                                        *)
(* Last modification by:                                  *)
(* Jose Antonio Alpizar Aguilar - 2016201868              *)
(* Pablo Josué Brenes Jiménez - 2016250460                *)
(* Luis José Castillo Valverde - 2016094804               *)
(* 22/12/2018                                             *)
(* ------------------------------------------------------ *)


open Token

(* Write the tokens in an XML file. (Receive: the scaner 
function, the buffer of the file being scanned and the name 
of the output file). *) 
val printTokens:
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string -> unit
