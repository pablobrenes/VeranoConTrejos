(* ------------------------------------- *)
(* Contextual Analyzer for Caml-Triangle *)
(* Implementation file                   *)
(*                                       *)
(* (c) 2006 Luis Leopoldo Pérez.         *)
(* Last modification: March 12, 2006     *)
(* ------------------------------------- *)

open Parser
open Ast
open ErrorReporter
open IdentificationTable


(* Reports an identifier as undeclared *)
let rec reportUndeclaredIdentifier a = match a with
    Identifier(i,s)          -> ErrorReporter.reportError (s ^ " is not declared") i.pos
  | CheckedIdentifier(b,_)   -> reportUndeclaredIdentifier b

(* Reports an operator as undeclared *)
let rec reportUndeclaredOperator a = match a with
    Operator(i,s)          -> ErrorReporter.reportError (s ^ " is not declared") i.pos
  | CheckedOperator(b,_)   -> reportUndeclaredOperator b

(* Returns the spelling of an identifier *)
let rec identifierName id = match id with 
    Identifier(_,s)        -> s
  | CheckedIdentifier(i,_) -> identifierName i
  
(* Returns the spelling of an operator *)
let rec operatorName id = match id with
    Operator(_,s)        -> s
  | CheckedOperator(o,_) -> operatorName o


(* Obtains the type of a field identifier *)
let rec visitFieldIdentifier ft id = match ft with
    MultipleFieldTypeDenoter(ix,i,t,mt) -> if ((String.compare (identifierName id) (identifierName i)) == 0) then
                                            t
                                          else 
                                            visitFieldIdentifier mt id
  | SingleFieldTypeDenoter(ix,i,t)     -> if ((String.compare (identifierName id) (identifierName i)) == 0) then
                                            t
                                          else
                                            ErrorTypeDenoter(ix)

 
(* Semantically checks the program, returning a "decorated" abstract syntax tree *)
let rec checkProgram a = match a with
    NullProgram  -> NullProgram
  | Program(ix,b) -> Program(ix, checkCommand b)


(* Commands *)
and checkCommand a = match a with

    (* Empty command - does nothing *)
    EmptyCommand(_)               -> a
    
    (* Assign command - checks if LHS is a variable and if both sides have the same types *)
  | AssignCommand(ix, v, e)      -> let vType = (checkVname v)
                                    and eType = (checkExpression e) in
                                        (match (vType,eType) with
                                           (CheckedVname(_,var,_,_,t),CheckedExpression(_,tt)) -> if (var == false) then
                                                                                                     ErrorReporter.reportError "LHS of assignment is not a variable" ix.pos;
                                                                                                  if ((compareTypes t tt) == false) then
                                                                                                     ErrorReporter.reportError "Assignment incompatibility" ix.pos;
                                                                                                  AssignCommand(ix,vType,eType)
                                         | _                                                  -> a)
                                    
  
    (* Call command - checks if the procedure exists and if the parameters are valid *)
  | CallCommand(ix, i, aps)       -> let iType = (checkIdentifier i) in
                                         (match iType with
                                           CheckedIdentifier(_,d)  -> (match !d with
                                                                         NullDeclaration                                             -> reportUndeclaredIdentifier i;
                                                                                                                                        a
                                                                       | ProcDeclaration(ix,_,fps,_)
                                                                       | FormalParameterDeclaration(ix,ProcFormalParameter(_,_,fps)) -> CallCommand(ix,iType,(checkActualParameterSequence aps fps))
                                                                       | _                                                           -> ErrorReporter.reportError ((identifierName i) ^ " is not a procedure identifier") ix.pos;
                                                                                                                                        a)
                                         | _                       -> a)
  
    (* Sequential command - checks both commands recursively *)
  | SequentialCommand(ix, c1, c2) -> let c1Type = (checkCommand c1)
                                     and c2Type = (checkCommand c2) in
                                         SequentialCommand(ix, c1Type, c2Type)
  
    (* Let (declaration) command - opens a new scope and inserts the new declaration into the identification table *)
  | LetCommand(ix, d, c)          -> IdentificationTable.openScope();
                                     let dType = (checkDeclaration d)
                                     and cType = (checkCommand c) in
                                         IdentificationTable.closeScope();
                                         LetCommand(ix, dType, cType)
  
    (* If command - checks if the expression is boolean, then checks both limbs recursively *)
  | IfCommand(ix, e, c1, c2)      -> let eType = (checkExpression e) in
                                         (match eType with
                                            CheckedExpression(_,BoolTypeDenoter(_)) -> ()
                                          | _                                       -> ErrorReporter.reportError "Boolean expression expected here" ix.pos);
                                         let c1Type = (checkCommand c1)
                                         and c2Type = (checkCommand c2) in
                                             IfCommand(ix, eType, c1Type, c2Type)
  
    (* While command - checks if the expression is boolean, then checks the inner command recursively *)
  | WhileCommand(ix, e, c)        -> let eType = (checkExpression e) in
                                         (match eType with
                                            CheckedExpression(_,BoolTypeDenoter(_)) -> ()
                                          | _                                       -> ErrorReporter.reportError "Boolean expression expected here" ix.pos);
                                         let cType = (checkCommand c) in
                                             WhileCommand(ix, eType, cType)
  
(* Expressions *)
and checkExpression e = match e with

    (* Empty expression - does nothing, returns a null type denoter *)
    EmptyExpression(_)              -> CheckedExpression(e, NullTypeDenoter)
    
    (* Integer expression - returns an integer type denoter *)
  | IntegerExpression(ix,il)        -> CheckedExpression(e, checkIntegerLiteral(il))
  
    (* Character expression - returns a character type denoter *)
  | CharacterExpression(ix, cl)     -> CheckedExpression(e, checkCharacterLiteral(cl))
  
    (* Value-or-variable name expression - returns the variable type *)
  | VnameExpression(ix, vn)         -> let vType = (checkVname vn) in
                                           (match vType with
                                              CheckedVname(_,_,_,_,t) -> CheckedExpression(VnameExpression(ix,vType), t)
                                            | _                       -> e)


    (* Call expression - checks if the function exists and if the parameters are valid *)
  | CallExpression(ix, i, ap)       -> let iType = (checkIdentifier i) in
                                           (match iType with
                                              CheckedIdentifier(_,d) -> (match !d with
                                                                           NullDeclaration                                              -> reportUndeclaredIdentifier i;
                                                                                                                                           e
                                                                         | FuncDeclaration(_,_,fps,t,_)
                                                                         | FormalParameterDeclaration(_,FuncFormalParameter(_,_,fps,t)) -> CheckedExpression(CallExpression(ix,iType,(checkActualParameterSequence ap fps)),t)
                                                                         | _                                                            -> ErrorReporter.reportError ((identifierName i) ^ " is not a function identifier") ix.pos;
                                                                                                                                           e)
                                                                         
                                            | _                      -> e)
  
    (* If expression - checks if the first expression is boolean, then checks if both limbs have the same type *)
  | IfExpression(ix, e1, e2, e3)    -> let e1Type = (checkExpression e1)
                                       and e2Type = (checkExpression e2)
                                       and e3Type = (checkExpression e3) in
                                           (match (e1Type, e2Type, e3Type) with
                                              (CheckedExpression(_,t1),CheckedExpression(_,t2),CheckedExpression(_,t3)) -> (match t1 with
                                                                                                                              BoolTypeDenoter(_) -> if ((compareTypes t2 t3) == false) then
                                                                                                                                                       ErrorReporter.reportError "Incompatible limbs in if-expression" ix.pos;
                                                                                                                                                    CheckedExpression(IfExpression(ix,e1Type,e2Type,e3Type),t2)
                                                                                                                            | _                  -> ErrorReporter.reportError "Boolean expression expected here" ix.pos;
                                                                                                                                                    e)
                                            | _                                                                          -> e)

    (* Let (declaration) expression - operns a new scope, checks the expression and inserts the new declaration into the identification table *)
  | LetExpression(ix, d, ex)        -> IdentificationTable.openScope();
                                       let dType = (checkDeclaration d)
                                       and eType = (checkExpression ex) in
                                           IdentificationTable.closeScope();
                                           (match eType with
                                              CheckedExpression(_,t) -> CheckedExpression(LetExpression(ix,dType,eType),t)
                                            | _                      -> e)

    (* Unary expression - checks if the operator exists, then checks if the operator and expression types are the same *)
  | UnaryExpression(ix, o, ex)      -> let eType = (checkExpression ex)
                                       and oType = (checkOperator o) in
                                           (match oType with
                                              CheckedOperator(_,d) -> (match !d with
                                                                         NullDeclaration                      -> reportUndeclaredOperator o;
                                                                                                                 CheckedExpression(e, ErrorTypeDenoter(ix))
                                                                       | UnaryOperatorDeclaration(_,o,t,tr)   -> (match eType with
                                                                                                                    CheckedExpression(_,tt) -> if ((compareTypes t tt) == false) then
                                                                                                                                                 ErrorReporter.reportError ("Wrong argument type for " ^ (operatorName o)) ix.pos;
                                                                                                                                               CheckedExpression(UnaryExpression(ix,oType,eType), tr)
                                                                                                                  | _                       -> e)
                                                                       | _                                    -> ErrorReporter.reportError ((operatorName o) ^ " is not an unary operator") ix.pos;
                                                                                                                 e)
                                            | _                    -> e)
  
    (* Binary expression - checks if the operator exists, then checks if the expression and operator types are the same *)
  | BinaryExpression(ix, e1, o, e2) -> let e1Type = (checkExpression e1)
                                       and e2Type = (checkExpression e2)
                                       and oType  = (checkOperator o) in
                                           (match oType with
                                              CheckedOperator(_,d) -> (match !d with
                                                                         NullDeclaration                         -> reportUndeclaredOperator o;
                                                                                                                    CheckedExpression(e, ErrorTypeDenoter(ix))
                                                                       | BinaryOperatorDeclaration(_,o,t1,t2,tr) -> (match (e1Type,e2Type) with
                                                                                                                       (CheckedExpression(_,te1),CheckedExpression(_,te2)) -> (match t1 with
                                                                                                                                                                                 AnyTypeDenoter(_) -> if ((compareTypes te1 te2) == false) then
                                                                                                                                                                                                         ErrorReporter.reportError ("Incompatible argument types for " ^ (operatorName o)) ix.pos
                                                                                                                                                                               | _                 -> if (((compareTypes t1 te1) == false) || ((compareTypes t2 te2) == false)) then
                                                                                                                                                                                                         ErrorReporter.reportError ("Wrong argument type for " ^ (operatorName o)) ix.pos)
                                                                                                                     | _ -> ());
                                                                                                                    CheckedExpression(BinaryExpression(ix,e1Type,oType,e2Type),tr)
                                                                                                                    
                                                                                                                       
                                                                       | _                                       -> ErrorReporter.reportError ((operatorName o) ^ " is not a binary operator") ix.pos;
                                                                                                                    e)
                                            | _                    -> e)
  
  
    (* Array expression - returns an array type denoter *)
  | ArrayExpression(ix, aa)         -> let aaType = (checkArrayAggregate aa) in
                                           (match aaType with
                                              CheckedArrayAggregate(SingleArrayAggregate(_,CheckedExpression(_,t)),i)
                                            | CheckedArrayAggregate(MultipleArrayAggregate(_,CheckedExpression(_,t),_),i) -> CheckedExpression(ArrayExpression(ix,aaType),ArrayTypeDenoter(ix,IntegerLiteral(ix,string_of_int i),t))
                                            | _                                                                           -> e)
                                            
    (* Record expression - returns a record type denoter *)
  | RecordExpression(ix, ra)        -> let raType = (checkRecordAggregate ra) in
                                           (match raType with
                                              CheckedRecordAggregate(_,t) -> CheckedExpression(RecordExpression(ix,raType),RecordTypeDenoter(ix,t))
                                            | _                           -> e)
                                        
    (* Already checked expression - does nothing *)
  | CheckedExpression(_,_)          -> e


(* Array Aggregates *)
and checkArrayAggregate a = match a with
    SingleArrayAggregate(ix,e)      -> CheckedArrayAggregate(SingleArrayAggregate(ix,(checkExpression e)),1)
    
  | MultipleArrayAggregate(ix,e,aa) -> let eType = (checkExpression e) 
                                       and aaType = (checkArrayAggregate aa) in
                                           (match eType with
                                              CheckedExpression(_,t) -> (match aaType with
                                                                           CheckedArrayAggregate(SingleArrayAggregate(_,CheckedExpression(_,tt)), i)
                                                                         | CheckedArrayAggregate(MultipleArrayAggregate(_,CheckedExpression(_,tt),_), i) -> if ((compareTypes t tt) == false) then
                                                                                                                                                             ErrorReporter.reportError ("Incompatible array-aggregate element") ix.pos;
                                                                                                                                                          CheckedArrayAggregate(MultipleArrayAggregate(ix, eType, aaType), i+1)
                                                                         | _                                                                             -> a)
                                            | _                      -> a)
                                        
  | CheckedArrayAggregate(_,_)      -> a


(* Record Aggregates *)
and checkRecordAggregate r = match r with
    SingleRecordAggregate(ix,i,e)      -> let eType = (checkExpression e) in
                                              (match eType with
                                                 CheckedExpression(_,t) -> CheckedRecordAggregate(SingleRecordAggregate(ix,i,eType),SingleFieldTypeDenoter(ix,i,t))
                                               | _                      -> r)
                                               
  | MultipleRecordAggregate(ix,i,e,r)  -> let eType = (checkExpression e)
                                          and rType = (checkRecordAggregate r) in
                                              (match rType with
                                                 CheckedRecordAggregate(_,t) -> let fType = (visitFieldIdentifier t i) in
                                                                                    (match fType with
                                                                                       ErrorTypeDenoter(_) -> ()
                                                                                     | _                   -> ErrorReporter.reportError ("Duplicate field " ^ (identifierName i) ^ " in record") ix.pos);
                                                                                    CheckedRecordAggregate(MultipleRecordAggregate(ix, i, eType, rType), MultipleFieldTypeDenoter(ix, i, (match eType with CheckedExpression(_,tt) -> tt | _ -> ErrorTypeDenoter(ix)), t))
                                               | _                           -> r)
                                                 
  | CheckedRecordAggregate(_,_)        -> r

(* Value-or-variable names *)
and checkVname v = match v with

    (* Simple value or variable names *)
    SimpleVname(ix, id) -> let idType = (checkIdentifier id) in
                               (match idType with
                                  CheckedIdentifier(_,d)  -> (match !d with
                                                                ConstDeclaration(_,_,e)                                   -> CheckedVname(SimpleVname(ix,idType), false, false, 0, (match (checkExpression e) with CheckedExpression(_,t) -> t | _ -> ErrorTypeDenoter(ix)))
                                                              | VarDeclaration(_,_,t)                                     -> CheckedVname(SimpleVname(ix,idType), true, false, 0, t)
                                                              | FormalParameterDeclaration(_,ConstFormalParameter(_,_,t)) -> CheckedVname(SimpleVname(ix,idType), false, false, 0, t)
                                                              | FormalParameterDeclaration(_,VarFormalParameter(_,_,t))   -> CheckedVname(SimpleVname(ix,idType), true, false, 0, t)
                                                              | _                                                         -> ErrorReporter.reportError ((identifierName id) ^ " is not a const or var identifier") ix.pos;
                                                                                                                             v)
                                | _                      -> v)

    (* Dot vnames - used over records *)
  | DotVname(ix, vn, id) -> let vType = (checkVname vn) in
                               (match vType with
                                  CheckedVname(_,var,_,_,t) -> (match t with
                                                                  RecordTypeDenoter(_,ft) -> let fType = (visitFieldIdentifier ft id) in
                                                                                                 (match fType with
                                                                                                    ErrorTypeDenoter(_) -> ErrorReporter.reportError ("No field " ^ (identifierName id) ^ " in this record type") ix.pos;
                                                                                                                           v
                                                                                                  | _                   -> CheckedVname(DotVname(ix,vType,(checkIdentifier id)), var, false, 0,fType))
                                                                | _                       -> ErrorReporter.reportError "Record expected here" ix.pos; 
                                                                                             v)
                                | _                         -> v)

    (* Subscript vnames - used over arrays *)
  | SubscriptVname(ix, vn, e) -> let vType = (checkVname vn)
                                 and eType = (checkExpression e) in
                                    (match vType with
                                       CheckedVname(_,var,_,_,t) -> (match t with
                                                                        ArrayTypeDenoter(_,_,tt) -> (match eType with
                                                                                                      CheckedExpression(ex, IntTypeDenoter(_))  -> CheckedVname(SubscriptVname(ix,vType,eType), var, false, 0, tt)
                                                                                                    | _                                         -> ErrorReporter.reportError "Integer expression expected here" ix.pos;
                                                                                                                                                        v)
                                                                      | _                        -> ErrorReporter.reportError "Array expected here" ix.pos;
                                                                                                    v)
                                       
                                     | _                         -> v)

    (* Already checked vnames - do nothing *)
  | CheckedVname(_,_,_,_,_) -> v


(* Declarations *)
and checkDeclaration d = match d with
    NullDeclaration                          -> d
    
  | ConstDeclaration(ix,i,e)                 -> let eType = (checkExpression e) in
                                                    if (IdentificationTable.exists (identifierName i)) then
                                                       ErrorReporter.reportError ("Identifier " ^ (identifierName i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifierName i) (ref (ConstDeclaration(ix,i,eType)));
                                                    !(IdentificationTable.retrieve (identifierName i))
  
  | VarDeclaration(ix,i,t)                   -> let tType = (checkTypeDenoter t) in
                                                    if (IdentificationTable.exists (identifierName i)) then
                                                       ErrorReporter.reportError ("Identifier " ^ (identifierName i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifierName i) (ref (VarDeclaration(ix,i,tType)));
                                                    !(IdentificationTable.retrieve (identifierName i))
  
  | ProcDeclaration(ix,i,fps,c)              -> if (IdentificationTable.exists (identifierName i)) then
                                                   ErrorReporter.reportError ("Identifier " ^ (identifierName i) ^ " already declared") ix.pos;
                                                IdentificationTable.enter (identifierName i) (ref (ProcDeclaration(ix,i,fps,c)));
                                                let elem = IdentificationTable.retrieveElement (identifierName i) in
                                                    IdentificationTable.openScope();
                                                    let fpsType = (checkFormalParameterSequence fps) in
                                                        elem.attr <- (ref (ProcDeclaration(ix,i,fpsType,c)));
                                                        let cType = (checkCommand c) in
                                                            IdentificationTable.closeScope();
                                                            elem.attr <- (ref (ProcDeclaration(ix,i,fpsType,cType)));
                                                            !(IdentificationTable.retrieve (identifierName i))
                                                    
  
  | FuncDeclaration(ix,i,fps,t,e)            -> let tType = (checkTypeDenoter t) in
                                                    if (IdentificationTable.exists (identifierName i)) then
                                                       ErrorReporter.reportError ("Identifier " ^ (identifierName i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifierName i) (ref (FuncDeclaration(ix,i,fps,tType,e)));
                                                    let elem = IdentificationTable.retrieveElement (identifierName i) in
                                                        IdentificationTable.openScope();
                                                        let fpsType = (checkFormalParameterSequence fps) in
                                                            elem.attr <- (ref (FuncDeclaration(ix,i,fpsType,tType,e)));
                                                            let eType = (checkExpression e) in
                                                                IdentificationTable.closeScope();
                                                                elem.attr <- (ref (FuncDeclaration(ix,i,fpsType,tType,eType)));
                                                                (match eType with
                                                                   CheckedExpression(_,t) -> if (t != tType) then
                                                                                                ErrorReporter.reportError ("Body of function " ^ (identifierName i) ^ " has wrong type") ix.pos
                                                                 | _ -> ());                                                                                             
                                                                !(IdentificationTable.retrieve (identifierName i))
  
  | TypeDeclaration(ix,i,t)                  -> let tType = (checkTypeDenoter t) in
                                                    if (IdentificationTable.exists (identifierName i)) then
                                                       ErrorReporter.reportError ("Identifier " ^ (identifierName i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifierName i) (ref (TypeDeclaration(ix,i,tType)));
                                                    !(IdentificationTable.retrieve (identifierName i))

  | UnaryOperatorDeclaration(ix,o,t1,t2)     -> d
  
  | BinaryOperatorDeclaration(ix,o,t1,t2,t3) -> d
  
  | FormalParameterDeclaration(_,_)          -> d
  
  | SequentialDeclaration(ix,d1,d2)          -> let d1Type = (checkDeclaration d1)
                                                and d2Type = (checkDeclaration d2) in
                                                    SequentialDeclaration(ix, d1Type, d2Type)


(* Actual Parameters *)
and checkActualParameter a f = match a with
    ConstActualParameter(ix,e) -> let eType = (checkExpression e) in
                                      (match f with
                                         ConstFormalParameter(_,_,t) -> (match eType with
                                                                           CheckedExpression(_,tt) -> if ((compareTypes t tt) == false) then
                                                                                                      begin
                                                                                                         ErrorReporter.reportError "Wrong type for const actual parameter" ix.pos;
                                                                                                         a
                                                                                                      end
                                                                                                      else
                                                                                                         ConstActualParameter(ix,eType)
                                                                         | _                       -> a)
                                       | _                           -> ErrorReporter.reportError "Const actual parameter not expected here" ix.pos;
                                                                        a)

  | VarActualParameter(ix,v)   -> let vType = (checkVname v) in
                                     (match vType with
                                        CheckedVname(_,false,_,_,_)  -> ErrorReporter.reportError "Actual parameter is not a variable" ix.pos;
                                                                        a
                                      | CheckedVname(_,true,_,_,tt)  -> (match f with
                                                                          VarFormalParameter(_,_,t) -> if ((compareTypes t tt) == false) then
                                                                                                       begin
                                                                                                          ErrorReporter.reportError "Wrong type for var actual parameter" ix.pos;
                                                                                                          a
                                                                                                       end
                                                                                                       else
                                                                                                          VarActualParameter(ix,vType)
                                                                        | _                        -> ErrorReporter.reportError "Var actual parameter not expected here" ix.pos;
                                                                                                      a)
                                      | _                           -> a)


  | ProcActualParameter(ix,id) -> let idType = (checkIdentifier id) in
                                     (match f with
                                        ProcFormalParameter(_,_,fp)  -> (match idType with
                                                                           CheckedIdentifier(i,d) -> (match !d with
                                                                                                        NullDeclaration                                            -> reportUndeclaredIdentifier(i);
                                                                                                                                                                      a
                                                                                                      | FormalParameterDeclaration(_,ProcFormalParameter(_,i,fps))
                                                                                                      | ProcDeclaration(_,i,fps,_)                                 -> if ((compareFPS fp fps) == false) then
                                                                                                                                                                         ErrorReporter.reportError ("Wrong signature for procedure " ^ (identifierName i)) ix.pos;
                                                                                                                                                                      ProcActualParameter(ix,idType)
                                                                                                      | _                                                          -> ErrorReporter.reportError ((identifierName i) ^ " is not a procedure identifier") ix.pos;
                                                                                                      	                                                              a)
                                                                         | _                      -> a)
                                                                           
                                      | _                            -> ErrorReporter.reportError "Proc actual parameter not expected here" ix.pos;
                                                                        a)
                                       
  
  | FuncActualParameter(ix,id) -> let idType = (checkIdentifier id) in
                                      (match f with
                                         FuncFormalParameter(_,_,fp,t) -> (match idType with
                                                                             CheckedIdentifier(i,d) -> (match !d with
                                                                                                          NullDeclaration               -> reportUndeclaredIdentifier(i);
                                                                                                                                           a
                                                                                                        | FormalParameterDeclaration(_,FuncFormalParameter(_,i,fps,tp))
                                                                                                        | FuncDeclaration(_,i,fps,tp,_) -> if ((compareFPS fp fps) == false) then
                                                                                                                                              ErrorReporter.reportError ("Wrong signature for function " ^ (identifierName i)) ix.pos
                                                                                                                                           else if ((compareTypes t tp) == false) then
                                                                                                                                              ErrorReporter.reportError ("Wrong type for function " ^ (identifierName i)) ix.pos;
                                                                                                                                           FuncActualParameter(ix,idType)
                                                                                                        | _                             -> ErrorReporter.reportError ((identifierName i) ^ " is not a function identifier") ix.pos;
                                                                                                                                           a)
                                                                           | _                      -> a)
                                       
                                       | _ -> ErrorReporter.reportError "Func actual parameter not expected here" ix.pos; 
                                              a)


(* Actual Parameter Sequences *)
and checkActualParameterSequence a f = match a with
    EmptyActualParameterSequence(ix)        -> (match f with 
                                                    EmptyFormalParameterSequence(_)           -> a
                                                  | _                                         -> ErrorReporter.reportError "Too few actual parameters" ix.pos;
                                                                                                 a)
                                                                                       
  | SingleActualParameterSequence(ix,b)     -> (match f with
                                                    SingleFormalParameterSequence(ixx,fp)    -> SingleActualParameterSequence(ixx, (checkActualParameter b fp))
                                                  | _                                        -> ErrorReporter.reportError "Incorrect number of actual parameters" ix.pos;
                                                                                                a)
                                                                                           
  | MultipleActualParameterSequence(ix,b,c) -> (match f with
                                                    MultipleFormalParameterSequence(ixx,fp,fps) -> let fpType = (checkActualParameter b fp)
                                                                                                   and fpsType = (checkActualParameterSequence c fps) in
                                                                                                       MultipleActualParameterSequence(ixx, fpType, fpsType)
                                                  | _                                           -> ErrorReporter.reportError "Too many actual parameters" ix.pos;
                                                                                                   a)


(* Formal Parameters *)
and checkFormalParameter f = match f with
    ConstFormalParameter(ix,i,t)    -> let tType = (checkTypeDenoter t) in
                                       if (IdentificationTable.exists (identifierName i)) then
                                           ErrorReporter.reportError ("Duplicated formal parameter " ^ (identifierName i)) ix.pos;
                                       let cfp = ConstFormalParameter(ix,i,tType) in
                                           IdentificationTable.enter (identifierName i) (ref (FormalParameterDeclaration(ix,cfp)));
                                           cfp
                                                                                  
  | VarFormalParameter(ix,i,t)      -> let tType = (checkTypeDenoter t) in
                                       if (IdentificationTable.exists (identifierName i)) then
                                           ErrorReporter.reportError ("Duplicated formal parameter " ^ (identifierName i)) ix.pos;
                                       let vfp = VarFormalParameter(ix,i,tType) in
                                           IdentificationTable.enter (identifierName i) (ref (FormalParameterDeclaration(ix,vfp)));
                                           vfp
                                       
  | ProcFormalParameter(ix,i,fps)   -> IdentificationTable.openScope();
                                       let fpsType = (checkFormalParameterSequence fps) in
                                           IdentificationTable.closeScope();
                                       if (IdentificationTable.exists (identifierName i)) then
                                           ErrorReporter.reportError ("Duplicated formal parameter " ^ (identifierName i)) ix.pos;
                                       let pfp = ProcFormalParameter(ix,i,fpsType) in
                                           IdentificationTable.enter (identifierName i) (ref (FormalParameterDeclaration(ix,pfp)));
                                           pfp

  | FuncFormalParameter(ix,i,fps,t) -> IdentificationTable.openScope();
                                       let fpsType = (checkFormalParameterSequence fps)
                                       and tType   = (checkTypeDenoter t) in
                                           IdentificationTable.closeScope();
                                       if (IdentificationTable.exists (identifierName i)) then
                                           ErrorReporter.reportError ("Duplicated formal parameter " ^ (identifierName i)) ix.pos;
                                       let ffp = FuncFormalParameter(ix,i,fpsType,tType) in
                                           IdentificationTable.enter (identifierName i) (ref (FormalParameterDeclaration(ix,ffp)));
                                           ffp

(* Formal Parameter Sequences *)
and checkFormalParameterSequence f = match f with
    EmptyFormalParameterSequence(_)            -> f
    
  | SingleFormalParameterSequence(ix,fp)       -> SingleFormalParameterSequence(ix, checkFormalParameter fp)
  
  | MultipleFormalParameterSequence(ix,fp,fps) -> let fpType = (checkFormalParameter fp)
                                                  and fpsType = (checkFormalParameterSequence fps) in
                                                      MultipleFormalParameterSequence(ix, fpType, fpsType)

(* Type Denoters *)
and checkTypeDenoter a = match a with
    NullTypeDenoter           -> a
    
  | ErrorTypeDenoter(_)       -> a
  
  | AnyTypeDenoter(_)         -> a
  
  | SimpleTypeDenoter(ix,i)   -> let b = (checkIdentifier i) in
                                     (match b with
                                        CheckedIdentifier(_,a) -> (match !a with
                                                                      NullDeclaration        -> reportUndeclaredIdentifier b;
                                                                                                ErrorTypeDenoter(ix)
                                                                    | TypeDeclaration(_,_,t) -> t
                                                                    | _                      -> ErrorReporter.reportError ((identifierName i) ^ " is not a type identifier") ix.pos;
                                                                                                ErrorTypeDenoter(ix))
                                      | _                      -> ErrorTypeDenoter(ix))
                                      
  | ArrayTypeDenoter(ix,il,t) -> if ((int_of_string (match il with IntegerLiteral(_,s) -> s)) == 0) then
                                    ErrorReporter.reportError "Arrays must not be empty" (match il with IntegerLiteral(is,_) -> is).pos;
                                 ArrayTypeDenoter(ix,il,checkTypeDenoter(t))
                                 
  | RecordTypeDenoter(ix,ft)  -> RecordTypeDenoter(ix, checkFieldTypeDenoter(ft))
  
  | BoolTypeDenoter(_)        -> a
  
  | IntTypeDenoter(_)         -> a
  
  | CharTypeDenoter(_)        -> a


(* Field Type Denoters *)
and checkFieldTypeDenoter a = match a with
    SingleFieldTypeDenoter(ix,i,t)      -> SingleFieldTypeDenoter(ix, i, (checkTypeDenoter t))
    
  | MultipleFieldTypeDenoter(ix,i,t,ft) -> let tType = (checkTypeDenoter t)
                                           and ftType = (checkFieldTypeDenoter ft) in
                                               MultipleFieldTypeDenoter(ix, i, tType, ftType)
  
(* Integer Literals *)
and checkIntegerLiteral a = match a with
    IntegerLiteral(i,_) -> let dx = (IdentificationTable.retrieve "Integer") in
                               (match !dx with
                                  TypeDeclaration(_,_,t) -> t
                                | _                      -> IntTypeDenoter(i))

(* Character Literals *)
and checkCharacterLiteral a = match a with
    CharacterLiteral(i,_) -> let dx = (IdentificationTable.retrieve "Char") in
                               (match !dx with
                                  TypeDeclaration(_,_,t) -> t
                                | _                      -> CharTypeDenoter(i))

(* Identifiers *)
and checkIdentifier a = match a with
    Identifier(_,s)          -> CheckedIdentifier(a, (IdentificationTable.retrieve s))
    
  | CheckedIdentifier(_,_)   -> a


(* Operators *)  
and checkOperator a = match a with
    Operator(_,s)            -> CheckedOperator(a, (IdentificationTable.retrieve s))
    
  | CheckedOperator(_,_)     -> a

(* Checks if two types are equivalent *)  
and compareTypes t1 t2 = match (t1,t2) with
    (IntTypeDenoter(_),IntTypeDenoter(_))
  | (CharTypeDenoter(_),CharTypeDenoter(_))
  | (AnyTypeDenoter(_),AnyTypeDenoter(_))
  | (BoolTypeDenoter(_),BoolTypeDenoter(_)) -> true
  
  | (SimpleTypeDenoter(_,i1),SimpleTypeDenoter(_,i2)) -> (match ((checkIdentifier i1),(checkIdentifier i2)) with
                                                            (CheckedIdentifier(_,d1),CheckedIdentifier(_,d2)) -> (match (!d1,!d2) with
                                                                                                                    (TypeDeclaration(_,_,tx1),TypeDeclaration(_,_,tx2)) -> (compareTypes tx1 tx2)
                                                                                                                  | _                                                   -> false)
                                                          | _                                                 -> false)
  | (ArrayTypeDenoter(_,il1,tx1),ArrayTypeDenoter(_,il2,tx2)) -> let size = (match (il1,il2) with
                                                                             (IntegerLiteral(_,s1),IntegerLiteral(_,s2)) -> ((String.compare s1 s2) == 0)) 
                                                                   in
                                                                   (size && (compareTypes tx1 tx2))
                                                                   
  | (RecordTypeDenoter(_,ft1),RecordTypeDenoter(_,ft2)) -> let rec compareFTs f1 f2 = (match (f1,f2) with
                                                               (SingleFieldTypeDenoter(_,i1,tx1),SingleFieldTypeDenoter(_,i2,tx2))               -> (compareTypes tx1 tx2) && ((String.compare (identifierName i1) (identifierName i2)) == 0)
                                                             | (MultipleFieldTypeDenoter(_,i1,tx1,ftx1),MultipleFieldTypeDenoter(_,i2,tx2,ftx2)) -> (compareTypes tx1 tx2) && ((String.compare (identifierName i1) (identifierName i2)) == 0) && (compareFTs ftx1 ftx2)
                                                             | _                                                                                 -> false) 
                                                             in
                                                               (compareFTs ft1 ft2)
                                                               
  | _  -> false
  
(* Compares two formal parameters and its types *)
and compareFP fp1 fp2 = match (fp1,fp2) with
    (ConstFormalParameter(_,_,t1),ConstFormalParameter(_,_,t2))
  | (VarFormalParameter(_,_,t1),VarFormalParameter(_,_,t2))               -> (compareTypes t1 t2)
  | (ProcFormalParameter(_,_,fps1),ProcFormalParameter(_,_,fps2))         -> (compareFPS fps1 fps2)
  | (FuncFormalParameter(_,_,fps1,t1),FuncFormalParameter(_,_,fps2,t2))   -> (compareFPS fps1 fps2) && (compareTypes t1 t2)
  | _                                                                     -> false

(* Compares two formal parameter sequences and its types *)  
and compareFPS fps1 fps2 = match (fps1,fps2) with
    (EmptyFormalParameterSequence(_),EmptyFormalParameterSequence(_))                          -> true
  | (SingleFormalParameterSequence(_,fp1),SingleFormalParameterSequence(_,fp2))                -> (compareFP fp1 fp2)
  | (MultipleFormalParameterSequence(_,fp1,fps1),MultipleFormalParameterSequence(_,fp2,fps2))  -> (compareFP fp1 fp2) && (compareFPS fps1 fps2)
  | _                                                                                          -> false
  