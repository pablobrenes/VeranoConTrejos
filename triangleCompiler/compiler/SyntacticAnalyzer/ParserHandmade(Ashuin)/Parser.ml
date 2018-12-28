open Ast
open ErrorReporter
open Lexing
open RuntimeEntity
open Token

exception Synt_error of string;;

let tokenToStr r = (match r with
   INTLITERAL(a)     ->  "integer literal"
  | CHARLITERAL(a)   ->  "character literal"
  | IDENTIFIER(a)    ->  "identifier"
  | OPERATOR(a)      ->  "operator"
  | ARRAY            ->  "array"
  | BEGIN            ->  "begin"
  | CONST            ->  "const"
  | DO               ->  "do"
  | ELSE             ->  "else"
  | END              ->  "end"
  | FUNC             ->  "func"
  | IF               ->  "if"
  | IN               ->  "in"
  | LET              ->  "let"
  | OF               ->  "of"
  | PROC             ->  "proc"
  | RECORD           ->  "record"
  | THEN             ->  "then"
  | TYPE             ->  "type"
  | VAR              ->  "var"
  | WHILE            ->  "while"
  | DOT              ->  "."
  | COLON            ->  ":"
  | SEMICOLON        ->  ";"
  | COMMA            ->  ","
  | BECOMES          ->  ":="
  | IS               ->  "~"
  | LPAREN           ->  "("
  | RPAREN           ->  ")"
  | LBRACKET         ->  "["
  | RBRACKET         ->  "]"
  | LCURLY           ->  "{"
  | RCURLY           ->  "}"
  | EOF              ->  "end of file");;


(*
///////////////////////////////////////////////////////////////////////////////
//
// Variables
//
///////////////////////////////////////////////////////////////////////////////
*)

let rec currentToken = ref (EOF)

(*
///////////////////////////////////////////////////////////////////////////////
//
// Funciones
//
///////////////////////////////////////////////////////////////////////////////
*)

and acceptIt scanFunc lb = (currentToken := scanFunc lb)

and accept expectedToken scanFunc lb = (
	if (!currentToken == expectedToken) then
		begin
			acceptIt scanFunc lb
		end
		else
		begin
			ErrorReporter.reportError (tokenToStr (!currentToken) ^ " expected here") lb.Lexing.lex_curr_p ;
			raise (Synt_error "Syntactical error")
		end
) 

(*
///////////////////////////////////////////////////////////////////////////////
//
// LITERALS
//
///////////////////////////////////////////////////////////////////////////////
*)

and parseIntegerLiteral scanFunc lb = (
	match !currentToken with
	| INTLITERAL(a) -> 
		begin
			let ast = IntegerLiteral ({pos=lb.lex_curr_p;run=NullRuntimeEntity}, a ) in
			acceptIt scanFunc lb;
			ast
		end
	| _ -> 
		begin 
			ErrorReporter.reportError (tokenToStr(!currentToken)^ "  expected here") lb.Lexing.lex_curr_p ;
			raise (Synt_error "Syntactical error")
		end
)

and parseCharLiteral scanFunc lb = ( 
	match !currentToken with
	| CHARLITERAL(a) -> 
		begin
			let ast = CharacterLiteral ({pos=lb.lex_curr_p;run=NullRuntimeEntity}, a ) in
				acceptIt scanFunc lb;
				ast
	    end
	| _ -> 
		begin 
			ErrorReporter.reportError (tokenToStr(!currentToken)^ "  expected here") lb.Lexing.lex_curr_p ;
			raise (Synt_error "Syntactical error")
	    end
)

and parseIdentifier scanFunc lb = (
	match !currentToken with
	| IDENTIFIER(a) -> 
		begin
			let ast = Identifier ( {pos=lb.lex_curr_p;run=NullRuntimeEntity}, a )in
				acceptIt scanFunc lb;
				ast
	    end
	| _ -> 
		begin 
			ErrorReporter.reportError (tokenToStr(!currentToken)^ "  expected here") lb.Lexing.lex_curr_p ;
			raise (Synt_error "Syntactical error")
	    end
)

and parseOperator scanFunc lb = (
	match !currentToken with
	| OPERATOR(a) -> 
		begin
			let ast = Operator ({pos=lb.lex_curr_p;run=NullRuntimeEntity}, a )in
				acceptIt scanFunc lb;
				ast
	    end
	| _ -> 
		begin 
			ErrorReporter.reportError (tokenToStr(!currentToken)^ "  expected here") lb.Lexing.lex_curr_p ;
			raise (Synt_error "Syntactical error")
	    end
)

(*
///////////////////////////////////////////////////////////////////////////////
//
// COMMANDS
//
///////////////////////////////////////////////////////////////////////////////
*)

and parseSingleCommand scanFunc lb = (
	match !currentToken with
    	IDENTIFIER(a) -> 
  			begin
    			let iAST = (parseIdentifier scanFunc lb) in
    				match !currentToken with
					    | LPAREN ->
					      begin
					        acceptIt scanFunc lb;
					        let apsAST = (parseActualParameterSequence scanFunc lb) in
					          accept RPAREN scanFunc lb;
					          CallCommand( {pos=lb.lex_curr_p;run=NullRuntimeEntity}, iAST, apsAST )
					      end
					    | _ ->
					      begin
					        let vAST = parseRestOfVname iAST scanFunc lb  in
					          accept BECOMES scanFunc lb;
					          let eAST = (parseExpression scanFunc lb) in
					            AssignCommand ({pos=lb.lex_curr_p;run=NullRuntimeEntity}, vAST, eAST)
					      end

  			end
       | BEGIN -> 
      		begin
    			acceptIt scanFunc lb;
    			let cAST = parseCommand scanFunc lb in
      			accept END scanFunc lb;
     			cAST 
      		end
       | LET -> 
       		begin
    			acceptIt scanFunc lb;
	    		let dAST = parseDeclaration scanFunc lb in
	      		accept IN scanFunc lb;
	      		let scAST = parseSingleCommand scanFunc lb in
	        	LetCommand ({pos=lb.lex_curr_p;run=NullRuntimeEntity}, dAST, scAST)
    		end
       | IF -> 
       		begin
    			acceptIt scanFunc lb;
			    let eAST = parseExpression scanFunc lb in
			    accept THEN scanFunc lb; 
			    let sc1AST = parseSingleCommand scanFunc lb in
			    accept ELSE scanFunc lb;
			    let sc2AST = parseSingleCommand scanFunc lb in
			    IfCommand({pos=lb.lex_curr_p;run=NullRuntimeEntity}, eAST, sc1AST, sc2AST)
			end
		| WHILE -> 
			begin
    			acceptIt scanFunc lb;
    			let eAST = parseExpression scanFunc lb in
      			accept DO scanFunc lb;
        		let scAST = parseSingleCommand scanFunc lb in
          		WhileCommand ( {pos=lb.lex_curr_p;run=NullRuntimeEntity}, eAST, scAST)
   			end
		| SEMICOLON 
		| END 
		| ELSE 
		| IN 
		| EOF -> 
		  EmptyCommand({pos=lb.lex_curr_p;run=NullRuntimeEntity})
		| _ ->
		  begin 
		    ErrorReporter.reportError (tokenToStr(!currentToken)^ "  cannot start a command") lb.Lexing.lex_curr_p ;
		    raise (Synt_error "Syntactical error")
		  end
)

and parseCommand scanFunc lb = (
  let c1AST = ref (parseSingleCommand scanFunc lb) in
    while (!currentToken == SEMICOLON) do
      acceptIt scanFunc lb;
      let c2AST = parseSingleCommand scanFunc lb in
        c1AST := SequentialCommand ({pos=lb.lex_curr_p;run=NullRuntimeEntity}, !c1AST, c2AST)   (* Le dimos vuelta*)     
    done;
    !c1AST
)

(*
///////////////////////////////////////////////////////////////////////////////
//
// EXPRESSIONS
//
///////////////////////////////////////////////////////////////////////////////
*)

and parseExpression scanFunc lb = (
    match !currentToken with
    | LET -> 
      acceptIt scanFunc lb;
      let dAST = parseDeclaration scanFunc lb in
        accept IN scanFunc lb;
        let eAST = parseExpression scanFunc lb in
          let expressionAST = LetExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, dAST, eAST) in
            expressionAST
    | IF -> 
      acceptIt scanFunc lb;
      let e1AST = parseExpression scanFunc lb in
        accept THEN scanFunc lb;
        let e2AST = parseExpression scanFunc lb in
          accept ELSE scanFunc lb;
          let e3AST = parseExpression scanFunc lb in
            let expressionAST = IfExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, e1AST, e2AST, e3AST) in
              expressionAST
    | _ -> 
      let expressionAST = parseSecondaryExpression scanFunc lb in
        expressionAST
  )

and parseRecordAggregate scanFunc lb = (
    let iAST = parseIdentifier scanFunc lb in 
      accept IS scanFunc lb;
      let eAST = parseExpression scanFunc lb in
        if ( !currentToken == COMMA ) then 
        begin
          acceptIt scanFunc lb;
          let aAST = parseRecordAggregate scanFunc lb in
            let aggregateAST = MultipleRecordAggregate({pos=lb.lex_curr_p;run=NullRuntimeEntity}, iAST, eAST, aAST) in
              aggregateAST
        end
        else
        begin
          let aggregateAST = SingleRecordAggregate({pos=lb.lex_curr_p;run=NullRuntimeEntity}, iAST, eAST) in
          aggregateAST
        end
  )

and parseArrayAggregate scanFunc lb = (
    let eAST = parseExpression scanFunc lb in
      if ( !currentToken == COMMA ) then 
      begin
        acceptIt scanFunc lb;
        let aAST = parseArrayAggregate scanFunc lb in
          let aggregateAST = MultipleArrayAggregate({pos=lb.lex_curr_p;run=NullRuntimeEntity}, eAST, aAST) in
            aggregateAST
      end
      else
      begin
        let aggregateAST = SingleArrayAggregate({pos=lb.lex_curr_p;run=NullRuntimeEntity}, eAST) in
          aggregateAST
      end
  )

and parsePrimaryExpression scanFunc lb = (
    match !currentToken with
    | INTLITERAL(a) -> 
      let i1AST = parseIntegerLiteral scanFunc lb in
        let expressionAST = IntegerExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, i1AST) in
          expressionAST
    | CHARLITERAL(a) -> 
      let c1AST = parseCharLiteral scanFunc lb in
      let expressionAST = CharacterExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, c1AST) in
          expressionAST
    | LBRACKET -> 
      acceptIt scanFunc lb;
      let aaAST = parseArrayAggregate scanFunc lb in
        accept RBRACKET scanFunc lb;
        let expressionAST = ArrayExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, aaAST) in
          expressionAST
    | LCURLY -> 
      acceptIt scanFunc lb;
      let raAST = parseRecordAggregate scanFunc lb in
          accept RCURLY scanFunc lb;
          let expressionAST = RecordExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, raAST) in
          expressionAST
    | IDENTIFIER(a) -> 
      let iAST = parseIdentifier scanFunc lb in 
        if ( !currentToken == LPAREN ) then
          begin
            acceptIt scanFunc lb;
            let apsAST = parseActualParameterSequence scanFunc lb in
                accept RPAREN scanFunc lb;
                let expressionAST = CallExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, iAST, apsAST) in
                  expressionAST
          end
        else
        begin
          let vAST = (parseRestOfVname iAST scanFunc lb) in
            let expressionAST = VnameExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, vAST) in
              expressionAST       
        end
    | OPERATOR(a) ->
      let opAST = parseOperator scanFunc lb in 
        let eAST = parsePrimaryExpression scanFunc lb in 
          let expressionAST = UnaryExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, opAST, eAST) in
            expressionAST
    | LPAREN ->
      acceptIt scanFunc lb;
      let expressionAST = parseExpression scanFunc lb in
        accept RPAREN scanFunc lb;
        expressionAST
    | _ -> 
      begin 
        ErrorReporter.reportError (tokenToStr(!currentToken)^ "  cannot start an expression") lb.Lexing.lex_curr_p ;
        raise (Synt_error "Syntactical error")
      end
  )

and parseSecondaryExpression scanFunc lb = (
    let expressionAST = ref (parsePrimaryExpression scanFunc lb) in 
      while (match !currentToken with
      			| OPERATOR(a) -> true
      			| _ -> false) do
        let opAST = parseOperator scanFunc lb in
          let e2AST = parsePrimaryExpression scanFunc lb in 
            expressionAST := BinaryExpression({pos=lb.lex_curr_p;run=NullRuntimeEntity}, !expressionAST, opAST, e2AST)
      done ;
      !expressionAST
  )



(*
///////////////////////////////////////////////////////////////////////////////
//
// VALUE-OR-VARIABLE NAMES
//
///////////////////////////////////////////////////////////////////////////////

*)


and parseVname scanFunc lb = 
	let iAST = parseIdentifier scanFunc lb in
		let vAST = parseRestOfVname iAST scanFunc lb in 
		vAST


and parseRestOfVname iAST scanFunc lb = (
        let vAST = ref (SimpleVname ({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST)) in
        while (!currentToken == DOT || !currentToken == LBRACKET) do
          if (!currentToken == DOT) then
          begin
            acceptIt scanFunc lb;
            let iAST = parseIdentifier scanFunc lb in
              vAST := DotVname({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, !vAST, iAST)
          end         
          else
          begin
            acceptIt scanFunc lb;
            let eAST = parseExpression scanFunc lb in
              accept RBRACKET scanFunc lb;
              vAST := SubscriptVname({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, !vAST, eAST)
          end
        done;
        !vAST
      )

(*
///////////////////////////////////////////////////////////////////////////////
//
// DECLARATIONS
//
///////////////////////////////////////////////////////////////////////////////
*)


and parseSingleDeclaration scanFunc lb = (
    match !currentToken with
    | CONST -> 
      acceptIt scanFunc lb;
      let iAST = parseIdentifier scanFunc lb in
        accept IS scanFunc lb;
        let eAST = parseExpression scanFunc lb in 
          let declarationAST = ConstDeclaration({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, eAST) in
            declarationAST
    | VAR -> 
      acceptIt scanFunc lb;
      let iAST = parseIdentifier scanFunc lb in
        accept COLON scanFunc lb;
        let tAST = parseTypeDenoter scanFunc lb in 
          let declarationAST = VarDeclaration({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, tAST) in
            declarationAST
    | PROC ->
      acceptIt scanFunc lb;
      let iAST = parseIdentifier scanFunc lb in 
        accept LPAREN scanFunc lb;
        let fpsAST = parseFormalParameterSequence scanFunc lb in
          accept RPAREN scanFunc lb;
          accept IS scanFunc lb;
          let cAST = parseSingleCommand scanFunc lb in 
            let declarationAST = ProcDeclaration({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, fpsAST, cAST) in
              declarationAST
    | FUNC ->
      acceptIt scanFunc lb;
      let iAST = parseIdentifier scanFunc lb in 
        accept LPAREN scanFunc lb;
        let fpsAST = parseFormalParameterSequence scanFunc lb in 
          accept RPAREN scanFunc lb;
          accept COLON scanFunc lb;
          let tAST = parseTypeDenoter scanFunc lb in 
            accept IS scanFunc lb;
            let eAST = parseExpression scanFunc lb in 
              let declarationAST = FuncDeclaration({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, fpsAST, tAST, eAST) in
                declarationAST
    | TYPE -> 
      acceptIt scanFunc lb;
      let iAST = parseIdentifier scanFunc lb in
        accept IS scanFunc lb;
        let tAST = parseTypeDenoter scanFunc lb in 
          let declarationAST = TypeDeclaration({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, tAST) in
            declarationAST
    | _ -> 
      ErrorReporter.reportError (tokenToStr(!currentToken)^ "  cannot start a declaration") lb.Lexing.lex_curr_p ;
      raise (Synt_error "Syntactical error")
  )

and parseDeclaration scanFunc lb = (
    let declarationAST = ref (parseSingleDeclaration scanFunc lb) in 
      while !currentToken == SEMICOLON do
        acceptIt scanFunc lb;
        let d2AST = parseSingleDeclaration scanFunc lb  in 
          declarationAST := SequentialDeclaration({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, !declarationAST, d2AST)
      done;
      !declarationAST
  )




(*
///////////////////////////////////////////////////////////////////////////////
//
// PARAMETERS
//
///////////////////////////////////////////////////////////////////////////////
*)

and parseProperFormalParameterSequence scanFunc lb = (
	let fpAST = parseFormalParameter scanFunc lb in
		if (!currentToken == COMMA) then
		begin
			acceptIt scanFunc lb;
			let fpsAST = parseProperFormalParameterSequence scanFunc lb in 
				let formalsAST = MultipleFormalParameterSequence({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, fpAST , fpsAST) in
					formalsAST
		end
		else
		begin
			let formalsAST = SingleFormalParameterSequence({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, fpAST) in
				formalsAST
		end
)

and parseFormalParameterSequence scanFunc lb = (
		if (!currentToken == RPAREN) then
		begin
			let formalsAST = EmptyFormalParameterSequence({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}) in
				formalsAST
		end
		else
		begin
			let formalsAST = parseProperFormalParameterSequence scanFunc lb in
				formalsAST
		end
)

and parseFormalParameter scanFunc lb = (
	match !currentToken with
	| IDENTIFIER(a) -> 
		let iAST = parseIdentifier scanFunc lb in
			accept COLON scanFunc lb;
			let tAST = parseTypeDenoter scanFunc lb in 
				let formalAST = ConstFormalParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, tAST) in 
					formalAST
	| VAR -> 
		acceptIt scanFunc lb;
		let iAST = parseIdentifier scanFunc lb in
			accept COLON scanFunc lb;
			let tAST = parseTypeDenoter scanFunc lb in 
				let formalAST = VarFormalParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, tAST) in 
					formalAST
	| PROC -> 
		acceptIt scanFunc lb;
		let iAST = parseIdentifier scanFunc lb in
			accept LPAREN scanFunc lb;
			let fpsAST = parseFormalParameterSequence scanFunc lb in 
				accept RPAREN scanFunc lb;
				let formalAST = ProcFormalParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST , fpsAST) in 
					formalAST
	| FUNC -> 
		acceptIt scanFunc lb; 
		let iAST = parseIdentifier scanFunc lb in
			accept LPAREN scanFunc lb;
			let fpsAST = parseFormalParameterSequence scanFunc lb in 
				accept RPAREN scanFunc lb;
				accept COLON scanFunc lb;
				let tAST = parseTypeDenoter scanFunc lb in 
					let formalAST = FuncFormalParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST, fpsAST, tAST) in 
						formalAST
	| _ -> 
		ErrorReporter.reportError (tokenToStr(!currentToken)^ "   cannot start a formal parameter") lb.Lexing.lex_curr_p ;
		raise (Synt_error "Syntactical error")
)

and parseActualParameterSequence scanFunc lb = (
	if ( !currentToken == RPAREN ) then 
	begin
		let actualsAST = EmptyActualParameterSequence({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}) in 
			actualsAST
	end
	else
	begin
		let actualsAST = parseProperActualParamererSequence scanFunc lb in 
			actualsAST
	end
)

and parseActualParameter scanFunc lb = (
	match !currentToken with
	| IDENTIFIER(a) 
	| INTLITERAL(a)
	| CHARLITERAL(a)
	| OPERATOR(a) ->
		let eAST = parseExpression scanFunc lb in 
		let actualAST = ConstActualParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, eAST) in
		  actualAST 
	| LET
	| IF
	| LPAREN
	| LBRACKET
	| LCURLY ->
		let eAST = parseExpression scanFunc lb in 
			let actualAST = ConstActualParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, eAST) in
				actualAST 
	| VAR ->
		acceptIt scanFunc lb;
		let iAST = parseVname scanFunc lb in 
			let actualAST = VarActualParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST) in
				actualAST
	| PROC ->
		acceptIt scanFunc lb;
		let iAST = parseIdentifier scanFunc lb in 
			let actualAST = ProcActualParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST) in
				actualAST
	| FUNC ->
		acceptIt scanFunc lb;
		let iAST = parseIdentifier scanFunc lb in 
			let actualAST = FuncActualParameter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST) in
				actualAST
	| _ -> 
		ErrorReporter.reportError (tokenToStr(!currentToken)^ "   cannot start an actual parameter") lb.Lexing.lex_curr_p ;
		raise (Synt_error "Syntactical error")
)

and parseProperActualParamererSequence scanFunc lb = (
	let apAST = parseActualParameter scanFunc lb in 
		if (!currentToken == COMMA) then
		begin
			acceptIt scanFunc lb;
			let apsAST = parseProperActualParamererSequence scanFunc lb in
				let actualsAST = MultipleActualParameterSequence({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, apAST, apsAST) in	
					actualsAST
		end
		else
		begin
			let actualsAST = SingleActualParameterSequence({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, apAST) in
				actualsAST
		end
)


(*
///////////////////////////////////////////////////////////////////////////////
//
// TYPE-DENOTERS
//
///////////////////////////////////////////////////////////////////////////////
*)

 and parseTypeDenoter scanFunc lb = (
    match !currentToken with
    | IDENTIFIER(a) -> 
      let iAST = parseIdentifier scanFunc lb in
        let typeAST = SimpleTypeDenoter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST) in 
          typeAST
    | ARRAY -> 
      acceptIt scanFunc lb;
      let ilAST = parseIntegerLiteral scanFunc lb in
        accept OF scanFunc lb;
        let tAST = parseTypeDenoter scanFunc lb in 
          let typeAST = ArrayTypeDenoter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, ilAST, tAST) in 
            typeAST
    | RECORD -> 
      acceptIt scanFunc lb;
      let fAST = parseFieldTypeDenoter scanFunc lb in
        accept END scanFunc lb;
        let typeAST = RecordTypeDenoter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, fAST) in
          typeAST
    | _ -> 
      ErrorReporter.reportError (tokenToStr(!currentToken)^ "   cannot start a type denoter") lb.Lexing.lex_curr_p ;
      raise (Synt_error "Syntactical error")
)

and parseFieldTypeDenoter scanFunc lb = (
	let iAST = parseIdentifier scanFunc lb in
		accept COLON scanFunc lb;
		let tAST = parseTypeDenoter scanFunc lb in 
		if (!currentToken == COMMA) then
			begin
				acceptIt scanFunc lb;
				let fAST = parseFieldTypeDenoter scanFunc lb in
					let fieldAST = MultipleFieldTypeDenoter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST , tAST, fAST) in	
						fieldAST
			end
			else
			begin
				let fieldAST = SingleFieldTypeDenoter({pos=lb.Lexing.lex_curr_p;run=NullRuntimeEntity}, iAST , tAST) in
					fieldAST
			end
)



(*
///////////////////////////////////////////////////////////////////////////////
//
// Programa Principal
//
///////////////////////////////////////////////////////////////////////////////
*)

and parseProgram scanFunc lb = (

	currentToken := (scanFunc lb);

	let astCommand = parseCommand scanFunc lb in
		if (!currentToken <> EOF) then
		begin
			ErrorReporter.reportError "End of File expected." lb.Lexing.lex_curr_p ;
			(*raise (Synt_error "End Of file expected");*)
			NullProgram

		end
		else
		begin
			let astProgram = Program ( {pos=lb.lex_curr_p;run=NullRuntimeEntity}, astCommand) in
				astProgram
		end
)