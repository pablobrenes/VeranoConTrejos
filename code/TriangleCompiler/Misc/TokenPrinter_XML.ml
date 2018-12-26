(* ------------------------------------------------------ *)
(* Tokens Drawing Library in a XML file for Caml-Triangle *)
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

(* This function given a char returns its equivalent in XML (this for reasons 
of compatibility in the print). *)
let transformOperator o = (
  match o with 
    '<' -> "&lt;"
  | '!' -> "&#33;"        
  | '\"' -> "&#34;"        
  | '#' -> "&#35;"        
  | '$' -> "&#36;"        
  | '%' -> "&#37;"        
  | '&' -> "&#38;"        
  | '\'' -> "&#39;"        
  | '(' -> "&#40;"        
  | ')' -> "&#41;"        
  | '*' -> "&#42;"        
  | '+' -> "&#43;"        
  | ',' -> "&#44;"        
  | '-' -> "&#45;"        
  | '.' -> "&#46;"        
  | '/' -> "&#47;"        
  | ':' -> "&#58;"        
  | ';' -> "&#59;"         
  | '=' -> "&#61;"        
  | '>' -> "&#62;"        
  | '?' -> "&#63;"        
  | '@' -> "&#64;"        
  | '[' -> "&#91;"        
  | '\\' -> "&#92;"        
  | ']' -> "&#93;"        
  | '^' -> "&#94;"        
  | '_' -> "&#95;"        
  | '`' -> "&#96;"        
  | '{' -> "&#123;"       
  | '|' -> "&#124;"      
  | '}' -> "&#125;"     
  | '~' -> "&#126;"
  | _   ->  (String.make 1 o) )

(* This function transforms a normal string to a compatible one to any format. 
This function receives the string to be changed, the character transformation
function, the lower limit to be transformed, the upper limit to be
transformed and the result (it is advised to use "") *)
let rec transformString stringBase transformChar index limit result =
  if (index = limit) then
    result
  else
    let charToTransform = stringBase.[index] in
    let transformedChar = transformChar charToTransform in
    let newResult = result ^ transformedChar in
    transformString stringBase transformChar (index + 1) limit newResult


(* This function given a string returns its equivalent in XML *)
let clean_String str = (
	transformString str transformOperator 0 (String.length str) "")

(* This function given to the token returns its equivalent in string xml, 
thus allowing to fill the body of the program. *)
let tokenToStr r = (
  match r with
    INTLITERAL(a)   -> "<IntLiteral value=\""  ^ (clean_String a) 
                    ^ "\"></IntLiteral>"
  | CHARLITERAL(a)  -> "<CharLiteral value=\"" ^ (clean_String a) 
                    ^ "\"></CharLiteral>"
  | IDENTIFIER(a)   -> "<Identifier value=\""  ^ (clean_String a) 
                    ^ "\"></Identifier>"
  | OPERATOR(a)     -> "<Operator value=\""    ^ (clean_String a) 
                    ^ "\"></Operator>"
  | ARRAY           -> "<Token value=\"array\"></Token>"
  | BEGIN           -> "<Token value=\"begin\"></Token>"
  | CONST           -> "<Token value=\"const\"></Token>"
  | DO              -> "<Token value=\"do\"></Token>"
  | ELSE            -> "<Token value=\"else\"></Token>"
  | END             -> "<Token value=\"end\"></Token>"
  | FUNC            -> "<Token value=\"func\"></Token>"
  | IF              -> "<Token value=\"if\"></Token>"
  | IN              -> "<Token value=\"in\"></Token>"
  | LET             -> "<Token value=\"let\"></Token>"
  | OF              -> "<Token value=\"of\"></Token>"
  | PROC            -> "<Token value=\"proc\"></Token>"
  | RECORD          -> "<Token value=\"record\"></Token>"
  | THEN            -> "<Token value=\"then\"></Token>"
  | TYPE            -> "<Token value=\"type\"></Token>"
  | VAR             -> "<Token value=\"var\"></Token>"
  | WHILE           -> "<Token value=\"while\"></Token>"
  | DOT             -> "<Token value=\".\"></Token>"
  | COLON           -> "<Token value=\":\"></Token>"
  | SEMICOLON       -> "<Token value=\";\"></Token>"
  | COMMA           -> "<Token value=\",\"></Token>"
  | BECOMES         -> "<Token value=\":=\"></Token>"
  | IS              -> "<Token value=\"~\"></Token>"
  | LPAREN          -> "<Token value=\"(\"></Token>"
  | RPAREN          -> "<Token value=\")\"></Token>"
  | LBRACKET        -> "<Token value=\"[\"></Token>"
  | RBRACKET        -> "<Token value=\"]\"></Token>"
  | LCURLY          -> "<Token value=\"{\"></Token>"
  | RCURLY          -> "<Token value=\"}\"></Token>"
  | EOF             -> "<Token value=\"end of file\"></Token>")


(* This function receives the function of scanner, the buffer of the
file that is being scanned and the handler of the file in which it is being
written). It is responsible for completing the body of the XML. *)
let rec printTokensAux scanner lexBuf chan = 
  let currentToken = scanner lexBuf in
  let tokenString = tokenToStr currentToken in
    output_string chan tokenString;
    output_string chan "\n";

  if (currentToken <> EOF) then
    printTokensAux scanner lexBuf chan

(* This function receives the function of scanner, the buffer of the file 
that is being scanned and the name of the file in which it will be written. 
Is responsible for creating the file, fill in the header and footer. *)
let printTokens scanner lexBuf fileName =
  try
    let chan = open_out fileName in
      output_string chan "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
      output_string chan "<Program>";
      printTokensAux scanner lexBuf chan;
      output_string chan "</Program>";
      close_out_noerr chan
  with Sys_error s -> printf "Couldn't write tokens file. (%s)\n" s