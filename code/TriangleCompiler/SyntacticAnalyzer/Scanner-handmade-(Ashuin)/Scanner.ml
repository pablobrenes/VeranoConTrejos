(* ------------------------------------ *)
(* Syntactic Analyzer for Caml-Triangle *)
(* Implementation file                  *)
(*                                      *)
(* @author  Ashuin Sharma Ch.           *)
(* Last modification: June 27,  2006    *)
(* ------------------------------------ *)

open Token
open ErrorReporter


(*Global Variables*)
exception Lex_error of string;;

let digits = [ '1' ; '2' ; '3' ; '4' ; '5' ; '6' ; '7' ; '8' ; '9' ; '0' ]
let lc_letters = [ 'a' ; 'b' ; 'c' ; 'd' ; 'e' ; 'f' ; 'g' ; 'h' ; 'i' ; 'j' ; 'k' ; 'l' ; 'm' ; 'n' ; 'o' ; 'p' ; 'q' ; 'r' ; 's' ; 't' ; 'u' ; 'v' ; 'w' ; 'x' ; 'y' ; 'z' ]
let uc_letters = [ 'A' ; 'B' ; 'C' ; 'D' ; 'E' ; 'F' ; 'G' ; 'H' ; 'I' ; 'J' ; 'K' ; 'L' ; 'M' ; 'N' ; 'O' ; 'P' ; 'Q' ; 'R' ; 'S' ; 'T' ; 'U' ; 'V' ; 'W' ; 'X' ; 'Y' ; 'Z' ]
let letters = lc_letters @ uc_letters
let op_characters = [ '+' ; '-' ; '*' ; '/' ; '=' ; '<' ; '>' ; '\\' ; '&' ; '@' ; '%' ; '^' ; '?' ]
let space_tab = [ ' ' ; '\t' ]
let blank = space_tab@['\n']
let graphic = digits@letters@op_characters@space_tab@['.' ; ':' ; ';' ; ',' ; '~' ; '(' ; ')' ; '[' ; ']' ; '{' ; '}' ; '_' ; '|' ; '!' ; '\'' ; '`' ; '"' ; '#' ; '$' ]
let currentSpelling = Buffer.create 100
let line = ref 1

(*Miscelaneus Methods*)


let rec is_in mem = function
[] -> false
| x::rest -> x = mem || is_in mem rest

let str_of_ch ch = (String.make 1 ch)

let isDigit = fun c ->
  is_in c digits

let isLetter = fun c ->
  is_in c letters

let isGraphic = fun c ->
  is_in c graphic
  
let isOperator = fun c ->
  is_in c op_characters

let isBlank = fun c ->
  is_in c blank

(* Funtion to change line and getChar *)

let getChar lb curr_char = 
	begin
		lb.Lexing.lex_curr_pos <- lb.Lexing.lex_curr_pos + 1;
		curr_char := Lexing.lexeme_char lb (lb.Lexing.lex_curr_pos);
		lb.Lexing.lex_curr_p <- {Lexing.pos_fname=" " ; 
		                         Lexing.pos_lnum= (!line) ;
							 	 Lexing.pos_bol=lb.Lexing.lex_curr_p.Lexing.pos_bol ;
							 	 Lexing.pos_cnum = lb.Lexing.lex_curr_pos}
	end

let changeLine lb =
	begin
		line := !line + 1;
			lb.Lexing.lex_curr_p <- {Lexing.pos_fname=" " ;
									 Lexing.pos_lnum= (!line) ;
									 Lexing.pos_bol=lb.Lexing.lex_curr_pos ;
									 Lexing.pos_cnum = lb.Lexing.lex_curr_pos};
	end


(* Reads a token from the buffer and returns it *)
(*val scanToken: Lexing.lexbuf -> token*)

let scanToken lb = 
  let curr_char = ref (Lexing.lexeme_char lb lb.Lexing.lex_curr_pos) in
    let next_source_char () =  
 	if (lb.Lexing.lex_curr_pos < (lb.Lexing.lex_buffer_len-1) ) then
	begin
		getChar lb curr_char
	end
	else
	begin
		curr_char := '\000';
	end	

	in 
	let takeIt () = ( 
		Buffer.add_char currentSpelling !curr_char;
		next_source_char(); 
	) 
	in 
	let take expCh = ( 
		if (!curr_char=expCh) then
			takeIt()
		else
		begin
			ErrorReporter.reportError ("Lexical Error. Expected " ^ str_of_ch(expCh) ^ " found " ^ str_of_ch(!curr_char)) lb.Lexing.lex_curr_p ;
			raise (Lex_error "invalid character")
		end;
	)
	in 
	let scanSeparator () = ( 
		if (!curr_char = '!') then 
		begin
			takeIt();
			while (isGraphic !curr_char) do
				takeIt()
			done;
			take '\n';
			changeLine lb
		end
		else
			if (isBlank !curr_char ) then
			begin
				if (!curr_char = '\n') then
				begin
					takeIt();
					changeLine lb
				end
			else 
			begin 
				takeIt() 
			end
		end;
	)

	and 
	scan () = (
		if (isLetter !curr_char) then 
		begin
			takeIt();
			while ((isLetter !curr_char) || (isDigit !curr_char)) do
				takeIt()
			done;
			match (Buffer.contents currentSpelling) with
			| "array" -> Token.ARRAY
			| "begin" -> Token.BEGIN
			| "const" -> Token.CONST
			| "do"    -> Token.DO
			| "else"  -> Token.ELSE
			| "end"   -> Token.END
			| "func"  -> Token.FUNC
			| "if"    -> Token.IF
			| "in"    -> Token.IN
			| "let"   -> Token.LET
			| "of"    -> Token.OF
			| "proc"  -> Token.PROC
			| "record"-> Token.RECORD
			| "then"  -> Token.THEN
			| "type"  -> Token.TYPE
			| "var"   -> Token.VAR
			| "while" -> Token.WHILE
			| _       -> Token.IDENTIFIER (Buffer.contents currentSpelling)
		end
		else if (isDigit !curr_char) then 
			begin
				takeIt();
				while (isDigit !curr_char) do
					takeIt()
				done;
				Token.INTLITERAL (Buffer.contents currentSpelling)
			end
		else if (!curr_char= '\'') then 
			begin
				takeIt();
				if (isGraphic !curr_char ) then
				begin
					takeIt();
					take '\'';
					Token.CHARLITERAL (Buffer.contents currentSpelling)
				end
				else 
				begin 
					ErrorReporter.reportError ("Lexical Error. Expected a graphic character found " ^ str_of_ch(!curr_char)) lb.Lexing.lex_curr_p ;
					raise (Lex_error "invalid character")
				end
			end
		else if (isOperator !curr_char) then
			begin
				takeIt();
				while(isOperator !curr_char) do
					takeIt()
				done;
				Token.OPERATOR (Buffer.contents currentSpelling) 
			end
		else
			begin 
				match (!curr_char) with
				| '.' -> begin takeIt(); Token.DOT end
				| ':' -> begin
						 	takeIt(); 
						 	match (!curr_char) with
							| '=' -> begin takeIt(); Token.BECOMES end
							|  _  -> Token.COLON 
					     end
				| ';' -> begin takeIt(); Token.SEMICOLON end
				| ',' -> begin takeIt(); Token.COMMA end
				| '~' -> begin takeIt(); Token.IS end
				| '(' -> begin takeIt(); Token.LPAREN end
				| ')' -> begin takeIt(); Token.RPAREN end 
				| '[' -> begin takeIt(); Token.LBRACKET end
				| ']' -> begin takeIt(); Token.RBRACKET end
				| '{' -> begin takeIt(); Token.LCURLY end
				| '}' -> begin takeIt(); Token.RCURLY end
				| '\000' -> begin takeIt(); Token.EOF end
				| _ -> begin
						ErrorReporter.reportError "Invalid Character found. " lb.Lexing.lex_curr_p ;
						raise (Lex_error "invalid character")
					   end
			end; 
	)

	in 
	while (!curr_char = '!' ||  (isBlank !curr_char)) do
		scanSeparator()
	done;

	Buffer.clear currentSpelling;
	scan();;
