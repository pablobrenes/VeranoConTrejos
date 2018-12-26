(* ------------------------------------------------------ *)
(* Tokens Drawing Library in a HTML file for Caml-Triangle*)
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

(* This variable stores the first part of the container that stores each 
token *)
let firstPrint =  ref "<div class=\"col-\">
      <div class=\"card text-white "

(* This variable stores the second part of the container that stores each
token *)
let secondPrint = ref " mb-3\" style=\"max-width: 20rem;\">
        <div class=\"card-header text-white bg-dark\">"

(* This variable stores the third part of the container that stores each
token *)
let thirdPrint = ref "</div>
        <div class=\"card-body\">
          <h5 class=\"card-title\">"

(* This variable stores the fourth part of the container that stores each
token *)
let fourthPrint = ref "</h5>
        </div>
      </div>
    </div>"

(* This function calls each of the parts of the container in addition to 
completing the missing elements, returning the string with the container 
of the token *)
let tokenToStr r = (
  match r with
    INTLITERAL(a)   -> !firstPrint ^ "bg-secondary" ^ !secondPrint ^
                    "IntLiteral" ^ !thirdPrint ^ a ^ !fourthPrint
  | CHARLITERAL(a)  -> !firstPrint ^ "bg-success" ^ !secondPrint ^
                    "CharLiteral" ^ !thirdPrint ^ a ^ !fourthPrint
  | IDENTIFIER(a)   -> !firstPrint ^ "bg-danger" ^ !secondPrint ^
                    "Identifier" ^ !thirdPrint ^ a ^ !fourthPrint
  | OPERATOR(a)     -> !firstPrint ^ "bg-warning" ^ !secondPrint ^
                    "Operator" ^ !thirdPrint ^ a ^ !fourthPrint
  | ARRAY           -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "array" ^ !fourthPrint
  | BEGIN           -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "begin" ^ !fourthPrint
  | CONST           -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "const" ^ !fourthPrint
  | DO              -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "do" ^ !fourthPrint
  | ELSE            -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "else" ^ !fourthPrint
  | END             -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "end" ^ !fourthPrint
  | FUNC            -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "func" ^ !fourthPrint
  | IF              -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "if" ^ !fourthPrint
  | IN              -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "in" ^ !fourthPrint
  | LET             -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "let" ^ !fourthPrint
  | OF              -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "of" ^ !fourthPrint
  | PROC            -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "proc" ^ !fourthPrint
  | RECORD          -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "record" ^ !fourthPrint
  | THEN            -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "then" ^ !fourthPrint
  | TYPE            -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "type" ^ !fourthPrint
  | VAR             -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "var" ^ !fourthPrint
  | WHILE           -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "while" ^ !fourthPrint
  | DOT             -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "." ^ !fourthPrint
  | COLON           -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ ":" ^ !fourthPrint
  | SEMICOLON       -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ ";" ^ !fourthPrint
  | COMMA           -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "," ^ !fourthPrint
  | BECOMES         -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ ":=" ^ !fourthPrint
  | IS              -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "~" ^ !fourthPrint
  | LPAREN          -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "(" ^ !fourthPrint
  | RPAREN          -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ ")" ^ !fourthPrint
  | LBRACKET        -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "[" ^ !fourthPrint
  | RBRACKET        -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "]" ^ !fourthPrint
  | LCURLY          -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "{" ^ !fourthPrint
  | RCURLY          -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "}" ^ !fourthPrint
  | EOF             -> !firstPrint ^ "bg-primary" ^ !secondPrint ^
                    "Token" ^ !thirdPrint ^ "end of file" ^ !fourthPrint) 

(* This function receives the scanner function, the buffer of the file
being scanned and the handler of the file in which it is being written.
It is responsible for filling the body of the HTML. *)
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
      output_string chan "<!DOCTYPE html>
<html lang=\"es\">
<head>
  <title>Tokens HTML result</title>
  <meta charset=\"utf-8\" />
  <link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css\" integrity=\"sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO\" crossorigin=\"anonymous\">
</head>
<body>
  <header>
   <h1>Tokens</h1>
   <p>The result of the generation of tokens is the following:</p>
 </header>
 <div class=\"container\">
  <div class=\"row\">";

      printTokensAux scanner lexBuf chan;
      output_string chan "</div>
</div>
<script src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\" integrity=\"sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo\" crossorigin=\"anonymous\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js\" integrity=\"sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49\" crossorigin=\"anonymous\"></script>
<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js\" integrity=\"sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy\" crossorigin=\"anonymous\"></script>
</body>
</html>";
      close_out_noerr chan
  with Sys_error s -> printf "Couldn't write tokens file. (%s)\n" s