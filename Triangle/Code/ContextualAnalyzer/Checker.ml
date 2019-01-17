(* ------------------------------------- *)
(* Contextual Analyzer for Caml-Triangle *)
(* Implementation file                   *)
(*                                       *)
(* (c) 2006 Luis Leopoldo P�rez.         *)
(* Last modification: March 12, 2006     *)
(* ------------------------------------- *)

open Parser
open Ast
open ErrorReporter
open IdentificationTable


(* Reports an identifier as undeclared *)
let rec report_undeclared_identifier a = match a with
    Identifier(i,s)          -> ErrorReporter.report_error (s ^ " is not declared") i.pos
  | Checked_identifier(b,_)   -> report_undeclared_identifier b

(* Reports an operator as undeclared *)
let rec report_undeclared_operator a = match a with
    Operator(i,s)          -> ErrorReporter.report_error (s ^ " is not declared") i.pos
  | Checked_operator(b,_)   -> report_undeclared_operator b

(* Returns the spelling of an identifier *)
let rec identifier_name id = match id with 
    Identifier(_,s)        -> s
  | Checked_identifier(i,_) -> identifier_name i
  
(* Returns the spelling of an operator *)
let rec operator_name id = match id with
    Operator(_,s)        -> s
  | Checked_operator(o,_) -> operator_name o


(* Obtains the type of a field identifier *)
let rec visit_field_identifier ft id = match ft with
    Multiple_field_type_denoter(ix,i,t,mt) -> if ((String.compare (identifier_name id) (identifier_name i)) == 0) then
                                            t
                                          else 
                                            visit_field_identifier mt id
  | Single_field_type_denoter(ix,i,t)     -> if ((String.compare (identifier_name id) (identifier_name i)) == 0) then
                                            t
                                          else
                                            Error_type_denoter(ix)

 
(* Semantically checks the program, returning a "decorated" abstract syntax tree *)
let rec check_program a = match a with
    Null_program  -> Null_program
  | Program(ix,b) -> Program(ix, check_command b)


(* Commands *)
and check_command a = match a with

    (* Empty command - does nothing *)
    Empty_command(_)               -> a
    
    (* Assign command - checks if LHS is a variable and if both sides have the same types *)
  | Assign_command(ix, v, e)      -> let v_type = (check_vname v)
                                    and e_type = (check_expression e) in
                                        (match (v_type,e_type) with
                                           (Checked_vname(_,var,_,_,t),Checked_expression(_,tt)) -> if (var == false) then
                                                                                                     ErrorReporter.report_error "LHS of assignment is not a variable" ix.pos;
                                                                                                  if ((compare_types t tt) == false) then
                                                                                                     ErrorReporter.report_error "Assignment incompatibility" ix.pos;
                                                                                                  Assign_command(ix,v_type,e_type)
                                         | _                                                  -> a)
                                    
  
    (* Call command - checks if the procedure exists and if the parameters are valid *)
  | Call_command(ix, i, aps)       -> let i_type = (check_identifier i) in
                                         (match i_type with
                                           Checked_identifier(_,d)  -> (match !d with
                                                                         Null_declaration                                             -> report_undeclared_identifier i;
                                                                                                                                        a
                                                                       | Proc_declaration(ix,_,fps,_)
                                                                       | Formal_parameter_declaration(ix,Proc_formal_parameter(_,_,fps)) -> Call_command(ix,i_type,(check_actual_parameter_sequence aps fps))
                                                                       | _                                                           -> ErrorReporter.report_error ((identifier_name i) ^ " is not a procedure identifier") ix.pos;
                                                                                                                                        a)
                                         | _                       -> a)
  
    (* Sequential command - checks both commands recursively *)
  | Sequential_command(ix, c1, c2) -> let c1Type = (check_command c1)
                                     and c2Type = (check_command c2) in
                                         Sequential_command(ix, c1Type, c2Type)
  
    (* Let (declaration) command - opens a new scope and inserts the new declaration into the identification table *)
  | Let_command(ix, d, c)          -> IdentificationTable.open_scope();
                                     let d_type = (check_declaration d)
                                     and c_type = (check_command c) in
                                         IdentificationTable.close_scope();
                                         Let_command(ix, d_type, c_type)
  
    (* If command - checks if the expression is boolean, then checks both limbs recursively *)
  | If_command(ix, e, c1, c2)      -> let e_type = (check_expression e) in
                                         (match e_type with
                                            Checked_expression(_,Bool_type_denoter(_)) -> ()
                                          | _                                       -> ErrorReporter.report_error "Boolean expression expected here" ix.pos);
                                         let c1Type = (check_command c1)
                                         and c2Type = (check_command c2) in
                                             If_command(ix, e_type, c1Type, c2Type)
  
    (* While command - checks if the expression is boolean, then checks the inner command recursively *)
  | While_command(ix, e, c)        -> let e_type = (check_expression e) in
                                         (match e_type with
                                            Checked_expression(_,Bool_type_denoter(_)) -> ()
                                          | _                                       -> ErrorReporter.report_error "Boolean expression expected here" ix.pos);
                                         let c_type = (check_command c) in
                                             While_command(ix, e_type, c_type)
  
(* Expressions *)
and check_expression e = match e with

    (* Empty expression - does nothing, returns a null type denoter *)
    Empty_expression(_)              -> Checked_expression(e, Null_type_denoter)
    
    (* Integer expression - returns an integer type denoter *)
  | Integer_expression(ix,il)        -> Checked_expression(e, check_integer_literal(il))
  
    (* Character expression - returns a character type denoter *)
  | Character_expression(ix, cl)     -> Checked_expression(e, check_character_literal(cl))
  
    (* Value-or-variable name expression - returns the variable type *)
  | Vname_expression(ix, vn)         -> let v_type = (check_vname vn) in
                                           (match v_type with
                                              Checked_vname(_,_,_,_,t) -> Checked_expression(Vname_expression(ix,v_type), t)
                                            | _                       -> e)


    (* Call expression - checks if the function exists and if the parameters are valid *)
  | Call_expression(ix, i, ap)       -> let i_type = (check_identifier i) in
                                           (match i_type with
                                              Checked_identifier(_,d) -> (match !d with
                                                                           Null_declaration                                              -> report_undeclared_identifier i;
                                                                                                                                           e
                                                                         | Func_declaration(_,_,fps,t,_)
                                                                         | Formal_parameter_declaration(_,Func_formal_parameter(_,_,fps,t)) -> Checked_expression(Call_expression(ix,i_type,(check_actual_parameter_sequence ap fps)),t)
                                                                         | _                                                            -> ErrorReporter.report_error ((identifier_name i) ^ " is not a function identifier") ix.pos;
                                                                                                                                           e)
                                                                         
                                            | _                      -> e)
  
    (* If expression - checks if the first expression is boolean, then checks if both limbs have the same type *)
  | If_expression(ix, e1, e2, e3)    -> let e1Type = (check_expression e1)
                                       and e2Type = (check_expression e2)
                                       and e3Type = (check_expression e3) in
                                           (match (e1Type, e2Type, e3Type) with
                                              (Checked_expression(_,t1),Checked_expression(_,t2),Checked_expression(_,t3)) -> (match t1 with
                                                                                                                              Bool_type_denoter(_) -> if ((compare_types t2 t3) == false) then
                                                                                                                                                       ErrorReporter.report_error "Incompatible limbs in if-expression" ix.pos;
                                                                                                                                                    Checked_expression(If_expression(ix,e1Type,e2Type,e3Type),t2)
                                                                                                                            | _                  -> ErrorReporter.report_error "Boolean expression expected here" ix.pos;
                                                                                                                                                    e)
                                            | _                                                                          -> e)

    (* Let (declaration) expression - operns a new scope, checks the expression and inserts the new declaration into the identification table *)
  | Let_expression(ix, d, ex)        -> IdentificationTable.open_scope();
                                       let d_type = (check_declaration d)
                                       and e_type = (check_expression ex) in
                                           IdentificationTable.close_scope();
                                           (match e_type with
                                              Checked_expression(_,t) -> Checked_expression(Let_expression(ix,d_type,e_type),t)
                                            | _                      -> e)

    (* Unary expression - checks if the operator exists, then checks if the operator and expression types are the same *)
  | Unary_expression(ix, o, ex)      -> let e_type = (check_expression ex)
                                       and o_type = (check_operator o) in
                                           (match o_type with
                                              Checked_operator(_,d) -> (match !d with
                                                                         Null_declaration                      -> report_undeclared_operator o;
                                                                                                                 Checked_expression(e, Error_type_denoter(ix))
                                                                       | Unary_operator_declaration(_,o,t,tr)   -> (match e_type with
                                                                                                                    Checked_expression(_,tt) -> if ((compare_types t tt) == false) then
                                                                                                                                                 ErrorReporter.report_error ("Wrong argument type for " ^ (operator_name o)) ix.pos;
                                                                                                                                               Checked_expression(Unary_expression(ix,o_type,e_type), tr)
                                                                                                                  | _                       -> e)
                                                                       | _                                    -> ErrorReporter.report_error ((operator_name o) ^ " is not an unary operator") ix.pos;
                                                                                                                 e)
                                            | _                    -> e)
  
    (* Binary expression - checks if the operator exists, then checks if the expression and operator types are the same *)
  | Binary_expression(ix, e1, o, e2) -> let e1Type = (check_expression e1)
                                       and e2Type = (check_expression e2)
                                       and o_type  = (check_operator o) in
                                           (match o_type with
                                              Checked_operator(_,d) -> (match !d with
                                                                         Null_declaration                         -> report_undeclared_operator o;
                                                                                                                    Checked_expression(e, Error_type_denoter(ix))
                                                                       | Binary_operator_declaration(_,o,t1,t2,tr) -> (match (e1Type,e2Type) with
                                                                                                                       (Checked_expression(_,te1),Checked_expression(_,te2)) -> (match t1 with
                                                                                                                                                                                 Any_type_denoter(_) -> if ((compare_types te1 te2) == false) then
                                                                                                                                                                                                         ErrorReporter.report_error ("Incompatible argument types for " ^ (operator_name o)) ix.pos
                                                                                                                                                                               | _                 -> if (((compare_types t1 te1) == false) || ((compare_types t2 te2) == false)) then
                                                                                                                                                                                                         ErrorReporter.report_error ("Wrong argument type for " ^ (operator_name o)) ix.pos)
                                                                                                                     | _ -> ());
                                                                                                                    Checked_expression(Binary_expression(ix,e1Type,o_type,e2Type),tr)
                                                                                                                    
                                                                                                                       
                                                                       | _                                       -> ErrorReporter.report_error ((operator_name o) ^ " is not a binary operator") ix.pos;
                                                                                                                    e)
                                            | _                    -> e)
  
  
    (* Array expression - returns an array type denoter *)
  | Array_expression(ix, aa)         -> let aa_type = (check_array_aggregate aa) in
                                           (match aa_type with
                                              Checked_array_aggregate(Single_array_aggregate(_,Checked_expression(_,t)),i)
                                            | Checked_array_aggregate(Multiple_array_aggregate(_,Checked_expression(_,t),_),i) -> Checked_expression(Array_expression(ix,aa_type),Array_type_denoter(ix,Integer_literal(ix,string_of_int i),t))
                                            | _                                                                           -> e)
                                            
    (* Record expression - returns a record type denoter *)
  | Record_expression(ix, ra)        -> let ra_type = (check_record_aggregate ra) in
                                           (match ra_type with
                                              Checked_record_aggregate(_,t) -> Checked_expression(Record_expression(ix,ra_type),Record_type_denoter(ix,t))
                                            | _                           -> e)
                                        
    (* Already checked expression - does nothing *)
  | Checked_expression(_,_)          -> e


(* Array Aggregates *)
and check_array_aggregate a = match a with
    Single_array_aggregate(ix,e)      -> Checked_array_aggregate(Single_array_aggregate(ix,(check_expression e)),1)
    
  | Multiple_array_aggregate(ix,e,aa) -> let e_type = (check_expression e) 
                                       and aa_type = (check_array_aggregate aa) in
                                           (match e_type with
                                              Checked_expression(_,t) -> (match aa_type with
                                                                           Checked_array_aggregate(Single_array_aggregate(_,Checked_expression(_,tt)), i)
                                                                         | Checked_array_aggregate(Multiple_array_aggregate(_,Checked_expression(_,tt),_), i) -> if ((compare_types t tt) == false) then
                                                                                                                                                             ErrorReporter.report_error ("Incompatible array-aggregate element") ix.pos;
                                                                                                                                                          Checked_array_aggregate(Multiple_array_aggregate(ix, e_type, aa_type), i+1)
                                                                         | _                                                                             -> a)
                                            | _                      -> a)
                                        
  | Checked_array_aggregate(_,_)      -> a


(* Record Aggregates *)
and check_record_aggregate r = match r with
    Single_record_aggregate(ix,i,e)      -> let e_type = (check_expression e) in
                                              (match e_type with
                                                 Checked_expression(_,t) -> Checked_record_aggregate(Single_record_aggregate(ix,i,e_type),Single_field_type_denoter(ix,i,t))
                                               | _                      -> r)
                                               
  | Multiple_record_aggregate(ix,i,e,r)  -> let e_type = (check_expression e)
                                          and r_type = (check_record_aggregate r) in
                                              (match r_type with
                                                 Checked_record_aggregate(_,t) -> let f_type = (visit_field_identifier t i) in
                                                                                    (match f_type with
                                                                                       Error_type_denoter(_) -> ()
                                                                                     | _                   -> ErrorReporter.report_error ("Duplicate field " ^ (identifier_name i) ^ " in record") ix.pos);
                                                                                    Checked_record_aggregate(Multiple_record_aggregate(ix, i, e_type, r_type), Multiple_field_type_denoter(ix, i, (match e_type with Checked_expression(_,tt) -> tt | _ -> Error_type_denoter(ix)), t))
                                               | _                           -> r)
                                                 
  | Checked_record_aggregate(_,_)        -> r

(* Value-or-variable names *)
and check_vname v = match v with

    (* Simple value or variable names *)
    Simple_vname(ix, id) -> let id_type = (check_identifier id) in
                               (match id_type with
                                  Checked_identifier(_,d)  -> (match !d with
                                                                Const_declaration(_,_,e)                                   -> Checked_vname(Simple_vname(ix,id_type), false, false, 0, (match (check_expression e) with Checked_expression(_,t) -> t | _ -> Error_type_denoter(ix)))
                                                              | Var_declaration(_,_,t)                                     -> Checked_vname(Simple_vname(ix,id_type), true, false, 0, t)
                                                              | Formal_parameter_declaration(_,Const_formal_parameter(_,_,t)) -> Checked_vname(Simple_vname(ix,id_type), false, false, 0, t)
                                                              | Formal_parameter_declaration(_,Var_formal_parameter(_,_,t))   -> Checked_vname(Simple_vname(ix,id_type), true, false, 0, t)
                                                              | _                                                         -> ErrorReporter.report_error ((identifier_name id) ^ " is not a const or var identifier") ix.pos;
                                                                                                                             v)
                                | _                      -> v)

    (* Dot vnames - used over records *)
  | Dot_vname(ix, vn, id) -> let v_type = (check_vname vn) in
                               (match v_type with
                                  Checked_vname(_,var,_,_,t) -> (match t with
                                                                  Record_type_denoter(_,ft) -> let f_type = (visit_field_identifier ft id) in
                                                                                                 (match f_type with
                                                                                                    Error_type_denoter(_) -> ErrorReporter.report_error ("No field " ^ (identifier_name id) ^ " in this record type") ix.pos;
                                                                                                                           v
                                                                                                  | _                   -> Checked_vname(Dot_vname(ix,v_type,(check_identifier id)), var, false, 0,f_type))
                                                                | _                       -> ErrorReporter.report_error "Record expected here" ix.pos; 
                                                                                             v)
                                | _                         -> v)

    (* Subscript vnames - used over arrays *)
  | Subscript_vname(ix, vn, e) -> let v_type = (check_vname vn)
                                 and e_type = (check_expression e) in
                                    (match v_type with
                                       Checked_vname(_,var,_,_,t) -> (match t with
                                                                        Array_type_denoter(_,_,tt) -> (match e_type with
                                                                                                      Checked_expression(ex, Int_type_denoter(_))  -> Checked_vname(Subscript_vname(ix,v_type,e_type), var, false, 0, tt)
                                                                                                    | _                                         -> ErrorReporter.report_error "Integer expression expected here" ix.pos;
                                                                                                                                                        v)
                                                                      | _                        -> ErrorReporter.report_error "Array expected here" ix.pos;
                                                                                                    v)
                                       
                                     | _                         -> v)

    (* Already checked vnames - do nothing *)
  | Checked_vname(_,_,_,_,_) -> v


(* Declarations *)
and check_declaration d = match d with
    Null_declaration                          -> d
    
  | Const_declaration(ix,i,e)                 -> let e_type = (check_expression e) in
                                                    if (IdentificationTable.exists (identifier_name i)) then
                                                       ErrorReporter.report_error ("Identifier " ^ (identifier_name i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifier_name i) (ref (Const_declaration(ix,i,e_type)));
                                                    !(IdentificationTable.retrieve (identifier_name i))
  
  | Var_declaration(ix,i,t)                   -> let t_type = (check_type_denoter t) in
                                                    if (IdentificationTable.exists (identifier_name i)) then
                                                       ErrorReporter.report_error ("Identifier " ^ (identifier_name i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifier_name i) (ref (Var_declaration(ix,i,t_type)));
                                                    !(IdentificationTable.retrieve (identifier_name i))
  
  | Proc_declaration(ix,i,fps,c)              -> if (IdentificationTable.exists (identifier_name i)) then
                                                   ErrorReporter.report_error ("Identifier " ^ (identifier_name i) ^ " already declared") ix.pos;
                                                IdentificationTable.enter (identifier_name i) (ref (Proc_declaration(ix,i,fps,c)));
                                                let elem = IdentificationTable.retrieve_element (identifier_name i) in
                                                    IdentificationTable.open_scope();
                                                    let fps_type = (check_formal_parameter_sequence fps) in
                                                        elem.attr <- (ref (Proc_declaration(ix,i,fps_type,c)));
                                                        let c_type = (check_command c) in
                                                            IdentificationTable.close_scope();
                                                            elem.attr <- (ref (Proc_declaration(ix,i,fps_type,c_type)));
                                                            !(IdentificationTable.retrieve (identifier_name i))
                                                    
  
  | Func_declaration(ix,i,fps,t,e)            -> let t_type = (check_type_denoter t) in
                                                    if (IdentificationTable.exists (identifier_name i)) then
                                                       ErrorReporter.report_error ("Identifier " ^ (identifier_name i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifier_name i) (ref (Func_declaration(ix,i,fps,t_type,e)));
                                                    let elem = IdentificationTable.retrieve_element (identifier_name i) in
                                                        IdentificationTable.open_scope();
                                                        let fps_type = (check_formal_parameter_sequence fps) in
                                                            elem.attr <- (ref (Func_declaration(ix,i,fps_type,t_type,e)));
                                                            let e_type = (check_expression e) in
                                                                IdentificationTable.close_scope();
                                                                elem.attr <- (ref (Func_declaration(ix,i,fps_type,t_type,e_type)));
                                                                (match e_type with
                                                                   Checked_expression(_,t) -> if (t != t_type) then
                                                                                                ErrorReporter.report_error ("Body of function " ^ (identifier_name i) ^ " has wrong type") ix.pos
                                                                 | _ -> ());                                                                                             
                                                                !(IdentificationTable.retrieve (identifier_name i))
  
  | Type_declaration(ix,i,t)                  -> let t_type = (check_type_denoter t) in
                                                    if (IdentificationTable.exists (identifier_name i)) then
                                                       ErrorReporter.report_error ("Identifier " ^ (identifier_name i) ^ " already declared") ix.pos;
                                                    IdentificationTable.enter (identifier_name i) (ref (Type_declaration(ix,i,t_type)));
                                                    !(IdentificationTable.retrieve (identifier_name i))

  | Unary_operator_declaration(ix,o,t1,t2)     -> d
  
  | Binary_operator_declaration(ix,o,t1,t2,t3) -> d
  
  | Formal_parameter_declaration(_,_)          -> d
  
  | Sequential_declaration(ix,d1,d2)          -> let d1Type = (check_declaration d1)
                                                and d2Type = (check_declaration d2) in
                                                    Sequential_declaration(ix, d1Type, d2Type)


(* Actual Parameters *)
and check_actual_parameter a f = match a with
    Const_actual_parameter(ix,e) -> let e_type = (check_expression e) in
                                      (match f with
                                         Const_formal_parameter(_,_,t) -> (match e_type with
                                                                           Checked_expression(_,tt) -> if ((compare_types t tt) == false) then
                                                                                                      begin
                                                                                                         ErrorReporter.report_error "Wrong type for const actual parameter" ix.pos;
                                                                                                         a
                                                                                                      end
                                                                                                      else
                                                                                                         Const_actual_parameter(ix,e_type)
                                                                         | _                       -> a)
                                       | _                           -> ErrorReporter.report_error "Const actual parameter not expected here" ix.pos;
                                                                        a)

  | Var_actual_parameter(ix,v)   -> let v_type = (check_vname v) in
                                     (match v_type with
                                        Checked_vname(_,false,_,_,_)  -> ErrorReporter.report_error "Actual parameter is not a variable" ix.pos;
                                                                        a
                                      | Checked_vname(_,true,_,_,tt)  -> (match f with
                                                                          Var_formal_parameter(_,_,t) -> if ((compare_types t tt) == false) then
                                                                                                       begin
                                                                                                          ErrorReporter.report_error "Wrong type for var actual parameter" ix.pos;
                                                                                                          a
                                                                                                       end
                                                                                                       else
                                                                                                          Var_actual_parameter(ix,v_type)
                                                                        | _                        -> ErrorReporter.report_error "Var actual parameter not expected here" ix.pos;
                                                                                                      a)
                                      | _                           -> a)


  | Proc_actual_parameter(ix,id) -> let id_type = (check_identifier id) in
                                     (match f with
                                        Proc_formal_parameter(_,_,fp)  -> (match id_type with
                                                                           Checked_identifier(i,d) -> (match !d with
                                                                                                        Null_declaration                                            -> report_undeclared_identifier(i);
                                                                                                                                                                      a
                                                                                                      | Formal_parameter_declaration(_,Proc_formal_parameter(_,i,fps))
                                                                                                      | Proc_declaration(_,i,fps,_)                                 -> if ((compare_f_p_s fp fps) == false) then
                                                                                                                                                                         ErrorReporter.report_error ("Wrong signature for procedure " ^ (identifier_name i)) ix.pos;
                                                                                                                                                                      Proc_actual_parameter(ix,id_type)
                                                                                                      | _                                                          -> ErrorReporter.report_error ((identifier_name i) ^ " is not a procedure identifier") ix.pos;
                                                                                                      	                                                              a)
                                                                         | _                      -> a)
                                                                           
                                      | _                            -> ErrorReporter.report_error "Proc actual parameter not expected here" ix.pos;
                                                                        a)
                                       
  
  | Func_actual_parameter(ix,id) -> let id_type = (check_identifier id) in
                                      (match f with
                                         Func_formal_parameter(_,_,fp,t) -> (match id_type with
                                                                             Checked_identifier(i,d) -> (match !d with
                                                                                                          Null_declaration               -> report_undeclared_identifier(i);
                                                                                                                                           a
                                                                                                        | Formal_parameter_declaration(_,Func_formal_parameter(_,i,fps,tp))
                                                                                                        | Func_declaration(_,i,fps,tp,_) -> if ((compare_f_p_s fp fps) == false) then
                                                                                                                                              ErrorReporter.report_error ("Wrong signature for function " ^ (identifier_name i)) ix.pos
                                                                                                                                           else if ((compare_types t tp) == false) then
                                                                                                                                              ErrorReporter.report_error ("Wrong type for function " ^ (identifier_name i)) ix.pos;
                                                                                                                                           Func_actual_parameter(ix,id_type)
                                                                                                        | _                             -> ErrorReporter.report_error ((identifier_name i) ^ " is not a function identifier") ix.pos;
                                                                                                                                           a)
                                                                           | _                      -> a)
                                       
                                       | _ -> ErrorReporter.report_error "Func actual parameter not expected here" ix.pos; 
                                              a)


(* Actual Parameter Sequences *)
and check_actual_parameter_sequence a f = match a with
    Empty_actual_parameter_sequence(ix)        -> (match f with 
                                                    Empty_formal_parameter_sequence(_)           -> a
                                                  | _                                         -> ErrorReporter.report_error "Too few actual parameters" ix.pos;
                                                                                                 a)
                                                                                       
  | Single_actual_parameter_sequence(ix,b)     -> (match f with
                                                    Single_formal_parameter_sequence(ixx,fp)    -> Single_actual_parameter_sequence(ixx, (check_actual_parameter b fp))
                                                  | _                                        -> ErrorReporter.report_error "Incorrect number of actual parameters" ix.pos;
                                                                                                a)
                                                                                           
  | Multiple_actual_parameter_sequence(ix,b,c) -> (match f with
                                                    Multiple_formal_parameter_sequence(ixx,fp,fps) -> let fp_type = (check_actual_parameter b fp)
                                                                                                   and fps_type = (check_actual_parameter_sequence c fps) in
                                                                                                       Multiple_actual_parameter_sequence(ixx, fp_type, fps_type)
                                                  | _                                           -> ErrorReporter.report_error "Too many actual parameters" ix.pos;
                                                                                                   a)


(* Formal Parameters *)
and check_formal_parameter f = match f with
    Const_formal_parameter(ix,i,t)    -> let t_type = (check_type_denoter t) in
                                       if (IdentificationTable.exists (identifier_name i)) then
                                           ErrorReporter.report_error ("Duplicated formal parameter " ^ (identifier_name i)) ix.pos;
                                       let cfp = Const_formal_parameter(ix,i,t_type) in
                                           IdentificationTable.enter (identifier_name i) (ref (Formal_parameter_declaration(ix,cfp)));
                                           cfp
                                                                                  
  | Var_formal_parameter(ix,i,t)      -> let t_type = (check_type_denoter t) in
                                       if (IdentificationTable.exists (identifier_name i)) then
                                           ErrorReporter.report_error ("Duplicated formal parameter " ^ (identifier_name i)) ix.pos;
                                       let vfp = Var_formal_parameter(ix,i,t_type) in
                                           IdentificationTable.enter (identifier_name i) (ref (Formal_parameter_declaration(ix,vfp)));
                                           vfp
                                       
  | Proc_formal_parameter(ix,i,fps)   -> IdentificationTable.open_scope();
                                       let fps_type = (check_formal_parameter_sequence fps) in
                                           IdentificationTable.close_scope();
                                       if (IdentificationTable.exists (identifier_name i)) then
                                           ErrorReporter.report_error ("Duplicated formal parameter " ^ (identifier_name i)) ix.pos;
                                       let pfp = Proc_formal_parameter(ix,i,fps_type) in
                                           IdentificationTable.enter (identifier_name i) (ref (Formal_parameter_declaration(ix,pfp)));
                                           pfp

  | Func_formal_parameter(ix,i,fps,t) -> IdentificationTable.open_scope();
                                       let fps_type = (check_formal_parameter_sequence fps)
                                       and t_type   = (check_type_denoter t) in
                                           IdentificationTable.close_scope();
                                       if (IdentificationTable.exists (identifier_name i)) then
                                           ErrorReporter.report_error ("Duplicated formal parameter " ^ (identifier_name i)) ix.pos;
                                       let ffp = Func_formal_parameter(ix,i,fps_type,t_type) in
                                           IdentificationTable.enter (identifier_name i) (ref (Formal_parameter_declaration(ix,ffp)));
                                           ffp

(* Formal Parameter Sequences *)
and check_formal_parameter_sequence f = match f with
    Empty_formal_parameter_sequence(_)            -> f
    
  | Single_formal_parameter_sequence(ix,fp)       -> Single_formal_parameter_sequence(ix, check_formal_parameter fp)
  
  | Multiple_formal_parameter_sequence(ix,fp,fps) -> let fp_type = (check_formal_parameter fp)
                                                  and fps_type = (check_formal_parameter_sequence fps) in
                                                      Multiple_formal_parameter_sequence(ix, fp_type, fps_type)

(* Type Denoters *)
and check_type_denoter a = match a with
    Null_type_denoter           -> a
    
  | Error_type_denoter(_)       -> a
  
  | Any_type_denoter(_)         -> a
  
  | Simple_type_denoter(ix,i)   -> let b = (check_identifier i) in
                                     (match b with
                                        Checked_identifier(_,a) -> (match !a with
                                                                      Null_declaration        -> report_undeclared_identifier b;
                                                                                                Error_type_denoter(ix)
                                                                    | Type_declaration(_,_,t) -> t
                                                                    | _                      -> ErrorReporter.report_error ((identifier_name i) ^ " is not a type identifier") ix.pos;
                                                                                                Error_type_denoter(ix))
                                      | _                      -> Error_type_denoter(ix))
                                      
  | Array_type_denoter(ix,il,t) -> if ((int_of_string (match il with Integer_literal(_,s) -> s)) == 0) then
                                    ErrorReporter.report_error "Arrays must not be empty" (match il with Integer_literal(is,_) -> is).pos;
                                 Array_type_denoter(ix,il,check_type_denoter(t))
                                 
  | Record_type_denoter(ix,ft)  -> Record_type_denoter(ix, check_field_type_denoter(ft))
  
  | Bool_type_denoter(_)        -> a
  
  | Int_type_denoter(_)         -> a
  
  | Char_type_denoter(_)        -> a


(* Field Type Denoters *)
and check_field_type_denoter a = match a with
    Single_field_type_denoter(ix,i,t)      -> Single_field_type_denoter(ix, i, (check_type_denoter t))
    
  | Multiple_field_type_denoter(ix,i,t,ft) -> let t_type = (check_type_denoter t)
                                           and ft_type = (check_field_type_denoter ft) in
                                               Multiple_field_type_denoter(ix, i, t_type, ft_type)
  
(* Integer Literals *)
and check_integer_literal a = match a with
    Integer_literal(i,_) -> let dx = (IdentificationTable.retrieve "Integer") in
                               (match !dx with
                                  Type_declaration(_,_,t) -> t
                                | _                      -> Int_type_denoter(i))

(* Character Literals *)
and check_character_literal a = match a with
    Character_literal(i,_) -> let dx = (IdentificationTable.retrieve "Char") in
                               (match !dx with
                                  Type_declaration(_,_,t) -> t
                                | _                      -> Char_type_denoter(i))

(* Identifiers *)
and check_identifier a = match a with
    Identifier(_,s)          -> Checked_identifier(a, (IdentificationTable.retrieve s))
    
  | Checked_identifier(_,_)   -> a


(* Operators *)  
and check_operator a = match a with
    Operator(_,s)            -> Checked_operator(a, (IdentificationTable.retrieve s))
    
  | Checked_operator(_,_)     -> a

(* Checks if two types are equivalent *)  
and compare_types t1 t2 = match (t1,t2) with
    (Int_type_denoter(_),Int_type_denoter(_))
  | (Char_type_denoter(_),Char_type_denoter(_))
  | (Any_type_denoter(_),Any_type_denoter(_))
  | (Bool_type_denoter(_),Bool_type_denoter(_)) -> true
  
  | (Simple_type_denoter(_,i1),Simple_type_denoter(_,i2)) -> (match ((check_identifier i1),(check_identifier i2)) with
                                                            (Checked_identifier(_,d1),Checked_identifier(_,d2)) -> (match (!d1,!d2) with
                                                                                                                    (Type_declaration(_,_,tx1),Type_declaration(_,_,tx2)) -> (compare_types tx1 tx2)
                                                                                                                  | _                                                   -> false)
                                                          | _                                                 -> false)
  | (Array_type_denoter(_,il1,tx1),Array_type_denoter(_,il2,tx2)) -> let size = (match (il1,il2) with
                                                                             (Integer_literal(_,s1),Integer_literal(_,s2)) -> ((String.compare s1 s2) == 0)) 
                                                                   in
                                                                   (size && (compare_types tx1 tx2))
                                                                   
  | (Record_type_denoter(_,ft1),Record_type_denoter(_,ft2)) -> let rec compare_f_ts f1 f2 = (match (f1,f2) with
                                                               (Single_field_type_denoter(_,i1,tx1),Single_field_type_denoter(_,i2,tx2))               -> (compare_types tx1 tx2) && ((String.compare (identifier_name i1) (identifier_name i2)) == 0)
                                                             | (Multiple_field_type_denoter(_,i1,tx1,ftx1),Multiple_field_type_denoter(_,i2,tx2,ftx2)) -> (compare_types tx1 tx2) && ((String.compare (identifier_name i1) (identifier_name i2)) == 0) && (compare_f_ts ftx1 ftx2)
                                                             | _                                                                                 -> false) 
                                                             in
                                                               (compare_f_ts ft1 ft2)
                                                               
  | _  -> false
  
(* Compares two formal parameters and its types *)
and compare_f_p fp1 fp2 = match (fp1,fp2) with
    (Const_formal_parameter(_,_,t1),Const_formal_parameter(_,_,t2))
  | (Var_formal_parameter(_,_,t1),Var_formal_parameter(_,_,t2))               -> (compare_types t1 t2)
  | (Proc_formal_parameter(_,_,fps1),Proc_formal_parameter(_,_,fps2))         -> (compare_f_p_s fps1 fps2)
  | (Func_formal_parameter(_,_,fps1,t1),Func_formal_parameter(_,_,fps2,t2))   -> (compare_f_p_s fps1 fps2) && (compare_types t1 t2)
  | _                                                                     -> false

(* Compares two formal parameter sequences and its types *)  
and compare_f_p_s fps1 fps2 = match (fps1,fps2) with
    (Empty_formal_parameter_sequence(_),Empty_formal_parameter_sequence(_))                          -> true
  | (Single_formal_parameter_sequence(_,fp1),Single_formal_parameter_sequence(_,fp2))                -> (compare_f_p fp1 fp2)
  | (Multiple_formal_parameter_sequence(_,fp1,fps1),Multiple_formal_parameter_sequence(_,fp2,fps2))  -> (compare_f_p fp1 fp2) && (compare_f_p_s fps1 fps2)
  | _                                                                                          -> false
  