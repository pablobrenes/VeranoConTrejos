(* ------------------------------------------------------ *)
(* Abstract Syntax Tree Drawing Library for Caml-Triangle *)
(* Implementation file                                    *)
(*                                                        *)
(* (c) 2006 Luis Leopoldo P�rez.                          *)
(* Last modification by:                                  *)
(* Jose Antonio Alpizar Aguilar - 2016201868              *)
(* Pablo Josu� Brenes Jim�nez - 2016250460                *)
(* Luis Jos� Castillo Valverde - 2016094804               *)
(* 22/12/2018                                             *)
(* ------------------------------------------------------ *)

open Ast
open Printf

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

(* Given a program, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)  
let rec writeProgram t chan = match t with
    NullProgram   -> output_string chan "<NullProgram/>\n"

  | Program (_,c) -> output_string chan "<Program>\n";
                     writeCommand c chan;
                     output_string chan "</Program>\n"
  
(* Given a command, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and writeCommand t chan = match t with
    EmptyCommand      (_)         -> output_string chan "<EmptyCommand/>\n"

  | AssignCommand     (_,v,e)     -> output_string chan "<AssignCommand>\n";
                                     writeVname v chan;
                                     writeExpression e chan;
                                     output_string chan "</AssignCommand>\n"

  | CallCommand       (_,i,aps)   -> output_string chan "<CallCommand>\n";
                                     writeIdentifier i chan;
                                     writeActualParameterSequence aps chan;
                                     output_string chan "</CallCommand>\n"

  | SequentialCommand (_,c1,c2)   -> output_string chan "<SequentialCommand>\n";
                                     writeCommand c1 chan;
                                     writeCommand c2 chan;
                                     output_string chan "</SequentialCommand>\n"

  | LetCommand        (_,d,c)     -> output_string chan "<LetCommand>\n";
                                     writeDeclaration d chan;
                                     writeCommand c chan;
                                     output_string chan "</LetCommand>\n"

  | IfCommand         (_,e,c1,c2) -> output_string chan "<IfCommand>\n";
                                     writeExpression e chan;
                                     writeCommand c1 chan;
                                     writeCommand c2 chan;
                                     output_string chan "</IfCommand>\n"

  | WhileCommand      (_,e,c)     -> output_string chan "<WhileCommand>\n";
                                     writeExpression e chan;
                                     writeCommand c chan;
                                     output_string chan "</WhileCommand>\n"

(* Given a expression, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and writeExpression t chan = match t with
    EmptyExpression     (_)          -> output_string chan "<EmptyExpression/>\n"

  | IntegerExpression   (_,il)       -> output_string chan "<IntegerExpression>\n";
                                        writeIntegerLiteral il chan;
                                        output_string chan "</IntegerExpression>\n"

  | CharacterExpression (_,cl)       -> output_string chan "<CharacterExpression>\n";
                                        writeCharacterLiteral cl chan;
                                        output_string chan "</CharacterExpression>\n"

  | VnameExpression     (_,v)        -> output_string chan "<VnameExpression>\n";
                                        writeVname v chan;
                                        output_string chan "</VnameExpression>\n"

  | CallExpression      (_,i,aps)    -> output_string chan "<CallExpression>\n";
                                        writeIdentifier i chan;
                                        writeActualParameterSequence aps chan;
                                        output_string chan "</CallExpression>\n"

  | IfExpression        (_,e1,e2,e3) -> output_string chan "<IfExpression>\n";
                                        writeExpression e1 chan;
                                        writeExpression e2 chan;
                                        writeExpression e3 chan;
                                        output_string chan "</IfExpression>\n"

  | LetExpression       (_,d,e)      -> output_string chan "<LetExpression>\n";
                                        writeDeclaration d chan;
                                        writeExpression e chan;
                                        output_string chan "</LetExpression>\n"

  | UnaryExpression     (_,o,e)      -> output_string chan "<UnaryExpression>\n";
                                        writeOperator o chan;
                                        writeExpression e chan;
                                        output_string chan "</UnaryExpression>\n"

  | BinaryExpression    (_,e1,o,e2)  -> output_string chan "<BinaryExpression>\n";
                                        writeExpression e1 chan;
                                        writeOperator o chan;
                                        writeExpression e2 chan;
                                        output_string chan "</BinaryExpression>\n"

  | ArrayExpression     (_,aa)       -> output_string chan "<ArrayExpression>\n";
                                        writeArrayAggregate aa chan;
                                        output_string chan "</ArrayExpression>\n"

  | RecordExpression    (_,ra)       -> output_string chan "<RecordExpression>\n";
                                        writeRecordAggregate ra chan;
                                        output_string chan "</RecordExpression>\n"

  | CheckedExpression   (e,t)        -> output_string chan ("<CheckedExpression>\n");
                                        writeExpression e chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</CheckedExpression>\n"
                                        
(* Given a aggregate, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and writeArrayAggregate t chan = match t with
    SingleArrayAggregate   (_,e)    -> output_string chan "<SingleArrayAggregate>\n";
                                       writeExpression e chan;
                                       output_string chan "</SingleArrayAggregate>\n"

  | MultipleArrayAggregate (_,e,aa) -> output_string chan "<MultipleArrayAggregate>\n";
                                       writeExpression e chan;
                                       writeArrayAggregate aa chan;
                                       output_string chan "</MultipleArrayAggregate>\n"

  | CheckedArrayAggregate  (aa,i)   -> output_string chan ("<CheckedArrayAggregate elementCount=\"" ^ string_of_int i ^ "\">\n");
                                       writeArrayAggregate aa chan;
                                       output_string chan "</CheckedArrayAggregate>\n"

(* Given a record, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and writeRecordAggregate t chan = match t with
    SingleRecordAggregate   (_,i,e)    -> output_string chan "<SingleRecordAggregate>\n";
                                          writeIdentifier i chan;
                                          writeExpression e chan;
                                          output_string chan "</SingleRecordAggregate>\n"

  | MultipleRecordAggregate (_,i,e,ra) -> output_string chan "<MultipleRecordAggregate>\n";
                                          writeIdentifier i chan;
                                          writeExpression e chan;
                                          writeRecordAggregate ra chan;
                                          output_string chan "</MultipleRecordAggregate>\n"

  | CheckedRecordAggregate  (ra,t)     -> output_string chan ("<CheckedRecordAggregate>\n");
                                          writeRecordAggregate ra chan;
                                          writeFieldTypeDenoter t chan;
                                          output_string chan "</CheckedRecordAggregate>\n"

(* Given a vname, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and writeVname t chan = match t with
    SimpleVname (_,i)            -> output_string chan "<SimpleVname>\n";
                                    writeIdentifier i chan;
                                    output_string chan "</SimpleVname>\n"

  | DotVname (_,v,i)             -> output_string chan "<DotVname>\n";
                                    writeVname v chan;
                                    writeIdentifier i chan;
                                    output_string chan "</DotVname>\n"

  | SubscriptVname (_,v,e)       -> output_string chan "<SubscriptVname>\n";
                                    writeVname v chan;
                                    writeExpression e chan;
                                    output_string chan "</SubscriptVname>\n"

  | CheckedVname (v,vr,ix,os,t)  -> output_string chan ("<CheckedVname variable=\"" ^ (string_of_bool vr) ^ "\" indexed=\"" ^ (string_of_bool ix) ^ "\" offset=\"" ^ (string_of_int os) ^ "\">\n");
                                    writeVname v chan;
                                    writeTypeDenoter t chan;
                                    output_string chan "</CheckedVname>\n"

(* Given a declaration, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and writeDeclaration t chan = match t with
    NullDeclaration                          -> output_string chan "</NullDeclaration>\n";
  | ConstDeclaration          (_,i,e)        -> output_string chan "<ConstDeclaration>\n";
                                                writeIdentifier i chan;
                                                writeExpression e chan;
                                                output_string chan "</ConstDeclaration>\n"

  | VarDeclaration            (_,i,t)        -> output_string chan "<VarDeclaration>\n";
                                                writeIdentifier i chan;
                                                writeTypeDenoter t chan;
                                                output_string chan "</VarDeclaration>\n"

  | ProcDeclaration           (_,i,fps,c)    -> output_string chan "<ProcDeclaration>\n";
                                                writeIdentifier i chan;
                                                writeFormalParameterSequence fps chan;
                                                writeCommand c chan;
                                                output_string chan "</ProcDeclaration>\n"

  | FuncDeclaration           (_,i,fps,t,e)  -> output_string chan "<FuncDeclaration>\n";
                                                writeIdentifier i chan;
                                                writeFormalParameterSequence fps chan;
                                                writeTypeDenoter t chan;
                                                writeExpression e chan;
                                                output_string chan "</FuncDeclaration>\n"

  | TypeDeclaration           (_,i,t)        -> output_string chan "<TypeDeclaration>\n";
                                                writeIdentifier i chan;
                                                writeTypeDenoter t chan;
                                                output_string chan "</TypeDeclaration>\n"

  | UnaryOperatorDeclaration  (_,o,t1,t2)    -> output_string chan "<UnaryOperatorDeclaration>\n";
                                                writeOperator o chan;
                                                writeTypeDenoter t1 chan;
                                                writeTypeDenoter t2 chan;
                                                output_string chan "</UnaryOperatorDeclaration>\n"

  | BinaryOperatorDeclaration (_,o,t1,t2,t3) -> output_string chan "<BinaryOperatorDeclaration>\n";
                                                writeOperator o chan;
                                                writeTypeDenoter t1 chan;
                                                writeTypeDenoter t2 chan;
                                                writeTypeDenoter t3 chan;
                                                output_string chan "</BinaryOperatorDeclaration>\n"
                                                
  | FormalParameterDeclaration(_,fp)         -> output_string chan "<FormalParameterDeclaration>\n";
                                                writeFormalParameter fp chan;
                                                output_string chan "</FormalParameterDeclaration>\n"

  | SequentialDeclaration     (_,d1,d2)      -> output_string chan "<SequentialDeclaration>\n";
                                                writeDeclaration d1 chan;
                                                writeDeclaration d2 chan;
                                                output_string chan "</SequentialDeclaration>\n"

(* Given a formal parameter, it will run through its tree recursively 
generating the openings and closures of tags in the corresponding places.. *)
and writeFormalParameter t chan = match t with
    ConstFormalParameter (_,i,t)     -> output_string chan "<ConstFormalParameter>\n";
                                        writeIdentifier i chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</ConstFormalParameter>\n"

  | VarFormalParameter (_,i,t)       -> output_string chan "<VarFormalParameter>\n";
                                        writeIdentifier i chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</VarFormalParameter>\n"

  | ProcFormalParameter (_,i,fps)    -> output_string chan "<ProcFormalParameter>\n";
                                        writeIdentifier i chan;
                                        writeFormalParameterSequence fps chan;
                                        output_string chan "</ProcFormalParameter>\n"

  | FuncFormalParameter (_,i,fps,t)  -> output_string chan "<FuncFormalParameter>\n";
                                        writeIdentifier i chan;
                                        writeFormalParameterSequence fps chan;
                                        writeTypeDenoter t chan;
                                        output_string chan "</FuncFormalParameter>\n"

(* Given a actual parameter, it will run through its tree recursively 
generating the openings and closures of tags in the corresponding places.. *)
and writeActualParameter t chan = match t with
    ConstActualParameter (_,e) -> output_string chan "<ConstActualParameter>\n";
                                  writeExpression e chan;
                                  output_string chan "</ConstActualParameter>\n"

  | VarActualParameter (_,v)   -> output_string chan "<VarActualParameter>\n";
                                  writeVname v chan;
                                  output_string chan "</VarActualParameter>\n"

  | ProcActualParameter (_,i)  -> output_string chan "<ProcActualParameter>\n";
                                  writeIdentifier i chan;
                                  output_string chan "</ProcActualParameter>\n"

  | FuncActualParameter (_,i)  -> output_string chan "<FuncActualParameter>\n";
                                  writeIdentifier i chan;
                                  output_string chan "</FuncActualParameter>\n"

(* Given a formal parameter sequence, it will run through its tree recursively 
generating the openings and closures of tags in the corresponding places.. *)
and writeFormalParameterSequence t chan = match t with
    EmptyFormalParameterSequence  (_)          -> output_string chan "<EmptyFormalParameterSequence/>\n"

  | SingleFormalParameterSequence (_,fp)       -> output_string chan "<SingleFormalParameterSequence>\n";
                                                  writeFormalParameter fp chan;
                                                  output_string chan "</SingleFormalParameterSequence>\n"

  | MultipleFormalParameterSequence (_,fp,fps) -> output_string chan "<MultipleFormalParameterSequence>\n";
                                                  writeFormalParameter fp chan;
                                                  writeFormalParameterSequence fps chan;
                                                  output_string chan "</MultipleFormalParameterSequence>\n"

(* Given a actual parameter sequence, it will run through its tree recursively 
generating the openings and closures of tags in the corresponding places.. *)
and writeActualParameterSequence t chan = match t with
    EmptyActualParameterSequence (_)           -> output_string chan "<EmptyActualParameterSequence/>\n"

  | SingleActualParameterSequence (_,ap)       -> output_string chan "<SingleActualParameterSequence>\n";
                                                  writeActualParameter ap chan;
                                                  output_string chan "</SingleActualParameterSequence>\n"

  | MultipleActualParameterSequence (_,ap,aps) -> output_string chan "<MultipleActualParameterSequence>\n";
                                                  writeActualParameter ap chan;
                                                  writeActualParameterSequence aps chan;
                                                  output_string chan "</MultipleActualParameterSequence>\n"

(* Given a type denoter, it will run through its tree recursively generating the
openings and closures of tags in the corresponding places.. *)
and writeTypeDenoter t chan = match t with
    NullTypeDenoter           -> output_string chan "<NullTypeDenoter/>\n"
  | ErrorTypeDenoter (_)      -> output_string chan "<ErrorTypeDenoter/>\n"

  | AnyTypeDenoter (_)        -> output_string chan "<AnyTypeDenoter/>\n"

  | SimpleTypeDenoter (_,i)   -> output_string chan "<SimpleTypeDenoter>\n";
                                 writeIdentifier i chan;
                                 output_string chan "</SimpleTypeDenoter>\n"

  | ArrayTypeDenoter (_,il,t) -> output_string chan "<ArrayTypeDenoter>\n";
                                 writeIntegerLiteral il chan;
                                 writeTypeDenoter t chan;
                                 output_string chan "</ArrayTypeDenoter>\n"

  | RecordTypeDenoter (_,ft)  -> output_string chan "<RecordTypeDenoter>\n";
                                 writeFieldTypeDenoter ft chan;
                                 output_string chan "</RecordTypeDenoter>\n"

  | BoolTypeDenoter (_)       -> output_string chan "<BoolTypeDenoter/>\n"

  | IntTypeDenoter (_)        -> output_string chan "<IntTypeDenoter/>\n"

  | CharTypeDenoter (_)       -> output_string chan "<CharTypeDenoter/>\n"

(* Given a field type denoter, it will run through its tree recursively 
generating the openings and closures of tags in the corresponding places.. *)
and writeFieldTypeDenoter t chan = match t with
    SingleFieldTypeDenoter (_,i,t)      -> output_string chan "<SingleFieldTypeDenoter>\n";
                                           writeIdentifier i chan;
                                           writeTypeDenoter t chan;
                                           output_string chan "</SingleFieldTypeDenoter>\n"

  | MultipleFieldTypeDenoter (_,i,t,ft) -> output_string chan "<MultipleFieldTypeDenoter>\n";
                                           writeIdentifier i chan;
                                           writeTypeDenoter t chan;
                                           writeFieldTypeDenoter ft chan;
                                           output_string chan "</MultipleFieldTypeDenoter>\n"

(* Given a integer literal this prints the XML version *)
and writeIntegerLiteral t chan = match t with
    IntegerLiteral (_,str) -> output_string chan ("<IntegerLiteral value=\"" ^ str ^ "\"/>\n")

(* Given a character literal this prints the XML version *)
and writeCharacterLiteral t chan = match t with
    CharacterLiteral (_,str) -> output_string chan ("<CharacterLiteral value=\"" ^ (clean_String str) ^ "\"/>\n")

(* Given a identifier this prints the XML version *)
and writeIdentifier t chan = match t with 
    Identifier (_,str)        -> output_string chan ("<Identifier value=\"" ^ str ^ "\"/>\n")

  | CheckedIdentifier (i,d)   -> output_string chan ("<CheckedIdentifier>\n");
                                 writeIdentifier i chan;
                                 output_string chan "</CheckedIdentifier>\n"

(* Given a operator this prints the XML version *)
and writeOperator t chan = match t with
    Operator (_,str)      -> output_string chan ("<Operator value=\"" ^ (clean_String str) ^ "\"/>\n")
  | CheckedOperator (o,d) -> output_string chan "<CheckedOperator>\n";
                             writeOperator o chan;
                             output_string chan "</CheckedOperator>\n"

(* This function receives the AST to be printed and the name of the file 
in which it will be written). It is responsible for creating the file *)
let writeXMLTree astree fname = 
    try
      let chan = open_out fname in
    	  output_string chan "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
          writeProgram astree chan;
          close_out_noerr chan
    with Sys_error s -> printf "Couldn't write XML tree file. (%s)\n" s

(* Note: The first argument of all printing functions is ignored because it
stores irrelevant information for printing. *)