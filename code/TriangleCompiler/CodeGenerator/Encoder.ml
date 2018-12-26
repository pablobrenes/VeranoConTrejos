(* --------------------------------- *)
(* Code Generator for Caml-Triangle  *)
(* Implementation file               *)
(*                                   *)
(* (c) 2006 Luis Leopoldo P�rez.     *)
(* Last modification: March 12, 2006 *)
(* --------------------------------- *)

open Ast
open RuntimeEntity
open IdentificationTable

(* Instruction type *)
type instruction = {op: int; r: int; n: int; d: int}

(* TABLE DETAILS - XML OUTPUT GENERATION *)

(* Table details type *)
type detail = {mutable dname: string; mutable dkind: string; mutable dsize: string; mutable dlevel: string; mutable ddispl: string; mutable dvalue: string}

type tableDetails = NullTableDetails
                  | TableDetails of detail list

(* The table itself *)                  
let table = ref NullTableDetails

(* Unboxes the list from the tableDetails type *)
let unboxTable() = match !table with
    NullTableDetails -> []
  | TableDetails(il) -> il

(* Adds a new identifier *)
let addTable dtail =
    table := TableDetails(unboxTable() @ [dtail])
    
(* Writes the current table details to the specified XML file *)
let writeXMLTable fname = 
    try
       let f = open_out fname in
           output_string f "<?xml version=\"1.0\" standalone=\"yes\"?>\n<tabledetails>\n";
           List.iter  (fun s -> output_string f ("<identifier name=\"" ^ s.dname ^ "\" kind=\"" ^ s.dkind ^ "\" size=\"" ^ s.dsize ^ "\" level=\"" ^ s.dlevel ^ "\" displacement=\"" ^ s.ddispl ^ "\" value=\"" ^ s.dvalue ^ "\"/>")) (unboxTable());
           output_string f "</tabledetails>\n";
           close_out f           
    with Sys_error s -> Printf.printf "Couldn't write XML table details file.\n"


(* Frame type *)
type frame = {lev: int; size: int}

(* TAM INFORMATION *)

let cb = 0     (* Code Store Registers *)
let pb = 1024
let pt = 1052

let nextAddr = ref cb
let codestore = Array.make 1024 {op=0; n=0; d=0; r=0} (* Code Store *)

let rCB =  0 (* Register Numbers *)
let rCT =  1
let rPB =  2
let rPT =  3
let rSB =  4
let rST =  5
let rHB =  6
let rHT =  7
let rLB =  8
let rL1 =  9
let rL2 =  10
let rL3 =  11
let rL4 =  12
let rL5 =  13
let rL6 =  14
let rCP =  15

let opLOAD   = 0 (* Operation codes *)
let opLOADA  = 1
let opLOADI  = 2
let opLOADL  = 3
let opSTORE  = 4
let opSTOREI = 5
let opCALL   = 6
let opCALLI  = 7
let opRETURN = 8
let opPUSH   = 10
let opPOP    = 11
let opJUMP   = 12
let opJUMPI  = 13
let opJUMPIF = 14
let opHALT   = 15

let booleanSize   =  1  (* Data Representation *)
let characterSize =  1
let integerSize   =  1
let addressSize   =  1
let closureSize   =  2
let linkDataSize  =  3
let falseRep      =  0
let trueRep       =  1
let maxintRep     =  32767

let idDisplacement      =  1 (* Addresses of primitive routines *)
let notDisplacement     =  2
let andDisplacement     =  3
let orDisplacement      =  4
let succDisplacement    =  5
let predDisplacement    =  6
let negDisplacement     =  7
let addDisplacement     =  8
let subDisplacement     =  9
let multDisplacement    =  10
let divDisplacement     =  11
let modDisplacement     =  12
let ltDisplacement      =  13
let leDisplacement      =  14
let geDisplacement      =  15
let gtDisplacement      =  16
let eqDisplacement      =  17
let neDisplacement      =  18
let eolDisplacement     =  19
let eofDisplacement     =  20
let getDisplacement     =  21
let putDisplacement     =  22
let geteolDisplacement  =  23
let puteolDisplacement  =  24
let getintDisplacement  =  25
let putintDisplacement  =  26
let newDisplacement     =  27
let disposeDisplacement =  28

let maxRoutineLevel = 7

(* Obtains the astInfo for declaration d *)
let obtainDeclAstInfo d = match d with
    NullDeclaration                       -> {pos=Lexing.dummy_pos; run=NullRuntimeEntity}
  | ConstDeclaration(ix,_,_)
  | VarDeclaration(ix,_,_)
  | ProcDeclaration(ix,_,_,_)
  | FuncDeclaration(ix,_,_,_,_)
  | TypeDeclaration(ix,_,_)
  | UnaryOperatorDeclaration(ix,_,_,_)
  | BinaryOperatorDeclaration(ix,_,_,_,_)
  | FormalParameterDeclaration(ix,_)
  | SequentialDeclaration(ix,_,_)         -> ix

(* Obtains the declaration for identifier i *)
let obtainIdentifierDeclaration i = match i with
    CheckedIdentifier(_,d) -> !d
  | Identifier(_,s)        -> !(retrieve s)


(* Obtains the size of runtime entity re *)
let obtainRuntimeEntitySize re = match re with
    NullRuntimeEntity -> 0
  | KnownValue(s,_)
  | UnknownValue(s,_)
  | KnownAddress(s,_)
  | UnknownAddress(s,_)
  | KnownRoutine(s,_)
  | UnknownRoutine(s,_)
  | PrimitiveRoutine(s,_)
  | EqualityRoutine(s,_)
  | Field(s,_)
  | TypeRepresentation(s)  -> s
  
(* Returns if a Vname is indexed *)
let isVnameIndexed v = match v with
    CheckedVname(_,_,b,_,_) -> b
  | _                       -> false

(* Returns the offset of a vname *)  
let obtainVnameOffset v = match v with
    CheckedVname(_,_,b,o,_) -> o
  | _                       -> 0

(* Returns the spelling of an identifier *)
let rec identifierName id = match id with 
    Identifier(_,s)        -> s
  | CheckedIdentifier(i,_) -> identifierName i
  
(* Obtains the type of a field identifier *)
let rec checkFieldIdentifier ft id = match ft with
    MultipleFieldTypeDenoter(ix,i,t,mt) -> if ((String.compare (identifierName id) (identifierName i)) == 0) then
                                            ix.run
                                          else 
                                            checkFieldIdentifier mt id
  | SingleFieldTypeDenoter(ix,i,t)     -> if ((String.compare (identifierName id) (identifierName i)) == 0) then
                                            ix.run
                                          else
                                            NullRuntimeEntity
  
    

  

(* Returns the runtime entity of a vname *)
let rec obtainVnameEntity v = match v with
    SimpleVname(_,i)         -> (obtainDeclAstInfo (obtainIdentifierDeclaration i)).run
  | DotVname(_,vn,_)         -> (obtainVnameEntity vn)
  | SubscriptVname(_,vn,_)   -> (obtainVnameEntity vn)
  | CheckedVname(vn,_,_,_,_) -> obtainVnameEntity vn

(* Appends an instruction, with the given fields, to the object code. *)
let emit xop xn xr xd =
    if (!nextAddr == pb) then
       ErrorReporter.reportRestriction "Too many instructions for code segment"
    else
    begin
       if (xn > 255) then
       begin
          ErrorReporter.reportRestriction "Length of operand can't exceed 255 words";
          Array.set codestore !nextAddr {op=xop; r=xr; n=255; d=xd}
       end
       else
          Array.set codestore !nextAddr {op=xop; r=xr; n=xn; d=xd};
       incr(nextAddr)
    end

(* Patches the d-field of the instruction at address addr. *)
let patch addr xd =
    let mx = codestore.(addr) in
        Array.set codestore addr {op=mx.op; r=mx.r; n=mx.n; d=xd}

(* Returns the register number appropriate for object code at currentLevel to address a data object at objectLevel. *)
let displayRegister currentLevel objectLevel =
    if (objectLevel == 0) then
       rSB
    else if (currentLevel - objectLevel <= 6) then
       rLB + currentLevel - objectLevel
    else
    begin
       ErrorReporter.reportRestriction "can't access data more than 6 levels out";
       rL6
    end

(* Programs *)
let rec visitProgram p f = match p with
    NullProgram  -> ()
  | Program(_,c) -> visitCommand c f


(* Commands *)
and visitCommand c f = match c with
    EmptyCommand(ix)              -> ()
    
  | AssignCommand(ix, v, e)       -> let vsize = (visitExpression e f) in  
                                         encodeStore v {lev = f.lev ; size = f.size + vsize} vsize;
                                         
  | CallCommand(ix, i, aps)       -> let asize = (visitActualParameterSequence aps f) in
                                         visitIdentifier i {lev = f.lev ; size = asize}
  
  | SequentialCommand(ix, c1, c2) -> visitCommand c1 f;
                                     visitCommand c2 f
                                     
  | LetCommand(ix, d, c)          -> let esize = (visitDeclaration d f) in
                                         visitCommand c {lev = f.lev ; size = f.size + esize};
                                         if (esize > 0) then
                                            emit opPOP 0 0 esize
                                            
  | IfCommand(ix, e, c1, c2)      -> let vsize = (visitExpression e f) in
                                         let jumpifaddr = !nextAddr in
                                             emit opJUMPIF falseRep rCB 0;
                                             visitCommand c1 f;
                                             let jumpaddr = !nextAddr in
                                                 emit opJUMP 0 rCB 0;
                                                 patch jumpifaddr !nextAddr;
                                                 visitCommand c2 f;
                                                 patch jumpaddr !nextAddr
  
  | WhileCommand(ix, e, c)        -> let jumpaddr = !nextAddr in
                                         emit opJUMP 0 rCB 0;
                                         let loopaddr = !nextAddr in
                                             visitCommand c f;
                                             patch jumpaddr !nextAddr;
                                             let ex = (visitExpression e f) in
                                                 emit opJUMPIF trueRep rCB loopaddr

(* Expressions *)
and visitExpression e f =    
    let visitInnerExpression e f vsize =
        (match e with
          EmptyExpression(ix)             -> 0
					       
        | IntegerExpression(ix, il)       -> emit opLOADL 0 0 (visitIntegerLiteral il);
                                             vsize
					     
        | CharacterExpression(ix, cl)     -> emit opLOADL 0 0 (visitCharacterLiteral cl);
                                             vsize
					                                         
        | VnameExpression(ix, vn)         -> encodeFetch vn f vsize;
                                             vsize
					                                         
        | CallExpression(ix, i, aps)      -> let args = (visitActualParameterSequence aps f) in
                                                 visitIdentifier i {lev = f.lev ; size = args};
                                                 vsize

        | IfExpression(ix, e1, e2, e3)    -> let ex1 = (visitExpression e1 f) in
                                                 let jumpifaddr = !nextAddr in
                                                     emit opJUMPIF falseRep rCB 0;
                                                     let ex2 = (visitExpression e2 f) in
                                                         let jumpaddr = !nextAddr in
                                                         emit opJUMP 0 rCB 0;
                                                         patch jumpifaddr !nextAddr;
                                                         let ex3 = (visitExpression e3 f) in
                                                             patch jumpaddr !nextAddr;
                                                             ex3
					    
        | LetExpression(ix, d, e)         -> let esize = (visitDeclaration d f) in
                                                 let f1 = {lev = f.lev ; size = f.size + esize} in
                                                     let usize = (visitExpression e f1) in
                                                         if (esize > 0) then
                                                            emit opPOP usize 0 esize;
                                                         usize

        | UnaryExpression(ix, o, e)       -> let ex = (visitExpression e f) in
                                                 visitOperator o {lev = f.lev ; size = vsize};
                                                 vsize

        | BinaryExpression(ix, e1, o, e2) -> let ex1 = (visitExpression e1 f) in
                                                 let f1 = {lev = f.lev ; size = f.size + ex1} in
                                                     let ex2 = (visitExpression e2 f1) in
                                                         let f2 = {lev = f.lev ; size = ex1 + ex2} in
                                                             visitOperator o f2;
                                                             vsize

        | ArrayExpression(ix, aa)         -> (visitArrayAggregate aa f)
					    
        | RecordExpression(ix, ra)        -> (visitRecordAggregate ra f)
        
        | _                               -> 0) in
          
           match e with
             CheckedExpression(ex, t)        -> let vsize = (visitTypeDenoter t) in
                                                    visitInnerExpression ex f vsize

           | _                               -> (visitInnerExpression e f 0)
 

(* Array Aggregates *)
and visitArrayAggregate aa f = match aa with
    SingleArrayAggregate(ix, e)        -> (visitExpression e f)
    
  | MultipleArrayAggregate(ix, e, aax) -> let fsize = (visitExpression e f) in  
                                                 let f1 = {lev = f.lev ; size = f.size + fsize} in
                                                     let rsize = (visitArrayAggregate aax f1) in
                                                        fsize + rsize
                                                        
  | CheckedArrayAggregate(aax, _)      -> (visitArrayAggregate aax f)

(* Record Aggregates *)    
and visitRecordAggregate ra f = match ra with
    SingleRecordAggregate(ix, i, e)        -> (visitExpression e f)
    
  | MultipleRecordAggregate(ix, i, e, rax) -> let fsize = (visitExpression e f) in
                                                  let f1 = {lev = f.lev ; size = f.size + fsize} in
                                                      let rsize = (visitRecordAggregate rax f1) in
                                                         fsize + rsize
                                                        
  | CheckedRecordAggregate(rax, _)         -> (visitRecordAggregate rax f)
  
  
(* Value-or-variable names *)
and visitVname v f = match v with
  | CheckedVname(vn, v, i, o, td) -> (match vn with
                                            SimpleVname(ix, i)            -> CheckedVname(vn, true, false, 0, td)
					  | DotVname(ix, vnx, i)          -> let vo = (visitVname vnx f) in
					                                         let oa = (match vo with
					                                             CheckedVname(_,_,_,_,t) -> (match t with
					                                                                           RecordTypeDenoter(_,tt) -> let mx = (visitFieldTypeDenoter tt 0) in
					                                                                                                          let yy = (checkFieldIdentifier tt i) in
					                                                                                                              (match yy with
					                                                                                                                 Field(s,o) -> o
					                                                                                                               | _          -> 0)
					                                                                         | _                       -> 0)
					                                           | _                       -> 0) in
					                                         let ofs = (obtainVnameOffset vo) + oa in
					                                             CheckedVname(vn, true, (isVnameIndexed vnx), ofs, td)
                                          | SubscriptVname(ix, vnx, e)    -> let vo = (visitVname vnx f)
                                                                             and esize = (visitTypeDenoter td) in
                                                                             let ofs = ref (obtainVnameOffset vo)
                                                                             and fram = ref f
                                                                             and isize = ref 0
                                                                             and ixed = ref (isVnameIndexed vnx) in
                                                                                 (match e with
                                                                                    CheckedExpression(IntegerExpression(_,il),_) -> ofs := !ofs + (visitIntegerLiteral il) * esize
                                                                                  | _                                            -> if (!ixed) then
                                                                                                                                       fram := {lev = f.lev ; size = f.size + integerSize};
                                                                                                                                    isize := (visitExpression e !fram);
                                                                                                                                    if (esize != 1) then 
                                                                                                                                    begin
                                                                                                                                       emit opLOADL 0 0 esize;
                                                                                                                                       emit opCALL rSB rPB multDisplacement
                                                                                                                                    end;
                                                                                                                                    if (!ixed) then
                                                                                                                                       emit opCALL rSB rPB addDisplacement
                                                                                                                                    else
                                                                                                                                       ixed := true);
                                                                                 CheckedVname(vn, true, !ixed, !ofs, td)

                                          | _                             -> vn)
  | _                             -> v
  

(* Declarations *)
and visitDeclaration d f = match d with
    NullDeclaration                             -> 0
    
  | ConstDeclaration(ix, i, e)                  -> let vsize = ref 0
                                                   and tdt = {dname=(identifierName i); dkind=""; dsize=""; dlevel=""; ddispl=""; dvalue=""} in
                                                       (match e with
                                                          CheckedExpression(CharacterExpression(_, cl),_)  -> let value = (visitCharacterLiteral cl) in
                                                                                                                  ix.run <- KnownValue(characterSize, value);
                                                                                                                  tdt.dkind <- "KnownValue";
                                                                                                                  tdt.dvalue <- string_of_int value;
                                                        | CheckedExpression(IntegerExpression(_, il),_)    -> let value = (visitIntegerLiteral il) in
                                                                                                                  ix.run <- KnownValue(integerSize, value);
                                                                                                                  tdt.dkind <- "KnownValue";
                                                                                                                  tdt.dvalue <- string_of_int value;
                                                        | _                                                -> vsize := (visitExpression e f);
                                                                                                              ix.run <- UnknownValue(!vsize, {level = f.lev ; displacement = f.size});
                                                                                                              tdt.dkind <- "UnknownValue";
                                                                                                              tdt.dlevel <- string_of_int f.lev;
                                                                                                              tdt.ddispl <- string_of_int f.size);
                                                       tdt.dsize <- string_of_int !vsize;
                                                       addTable tdt;
                                                       !vsize
                                                       
  | VarDeclaration(ix, i, t)                    -> let esize = (visitTypeDenoter t) in
                                                       emit opPUSH 0 0 esize;
                                                       ix.run <- KnownAddress(addressSize, {level = f.lev ; displacement = f.size});
                                                       addTable {dname=(identifierName i); dkind="KnownAddress"; dsize=(string_of_int addressSize); dlevel=(string_of_int f.lev); ddispl=(string_of_int f.size); dvalue=""};
                                                       esize
                                                       
  | ProcDeclaration(ix, i, fps, c)              -> let jaddr = !nextAddr 
                                                   and args = ref 0 in
                                                       emit opJUMP 0 rCB 0;
                                                       ix.run <- KnownRoutine(closureSize, {level = f.lev ; displacement = !nextAddr});
                                                       if (f.lev == maxRoutineLevel) then
                                                          ErrorReporter.reportRestriction "Can't nest routines so deeply"
                                                       else
                                                       begin
                                                          let f1 = {lev = f.lev + 1 ; size = 0} in
                                                              args := (visitFormalParameterSequence fps f1);
                                                              let f2 = {lev = f.lev + 1 ; size = linkDataSize} in
                                                                  (visitCommand c f2)
                                                       end;
                                                       emit opRETURN 0 0 !args;                                                       
                                                       patch jaddr !nextAddr;
                                                       addTable {dname=(identifierName i); dkind="KnownRoutine"; dsize=(string_of_int closureSize); dlevel=(string_of_int f.lev); ddispl=(string_of_int jaddr); dvalue=""};
                                                       0
                                               
  | FuncDeclaration(ix, i, fps, t, e)           -> let jaddr = !nextAddr
                                                   and args = ref 0
                                                   and vals = ref 0 in
                                                       emit opJUMP 0 rCB 0;
                                                       ix.run <- KnownRoutine(closureSize, {level = f.lev ; displacement = !nextAddr});
                                                       if (f.lev == maxRoutineLevel) then
                                                          ErrorReporter.reportRestriction "Can't nest routines so deeply"
                                                       else
                                                       begin
                                                          let f1 = {lev = f.lev + 1 ; size = 0} in
                                                              args := (visitFormalParameterSequence fps f1);
                                                              let f2 = { lev = f.lev + 1 ; size = linkDataSize} in
                                                                  vals := (visitExpression e f2)
                                                       end;
                                                       emit opRETURN !vals 0 !args;
                                                       patch jaddr !nextAddr;
                                                       addTable {dname=(identifierName i); dkind="KnownRoutine"; dsize=(string_of_int closureSize); dlevel=(string_of_int f.lev); ddispl=(string_of_int jaddr); dvalue=""};
                                                       0
                                                       
  | TypeDeclaration(ix, i, t)                   -> let x = (visitTypeDenoter t) in
                                                   0
  
  | UnaryOperatorDeclaration(ix, o, t1, t2)     -> 0
  
  | BinaryOperatorDeclaration(ix,o, t1, t2, t3) -> 0
  
  | FormalParameterDeclaration(ix, fp)          -> (visitFormalParameter fp f)
  
  | SequentialDeclaration(ix, d1, d2)           -> let esize1 = (visitDeclaration d1 f) in
                                                       let f1 = { lev = f.lev ; size = f.size + esize1 } in
                                                           let esize2 = (visitDeclaration d2 f1) in
                                                               esize1 + esize2

(* Formal parameters *)
and visitFormalParameter fp f = match fp with
    ConstFormalParameter(ix, i, t)     -> let vsize = (visitTypeDenoter t) in
                                              ix.run <- UnknownValue(vsize, {level=f.lev; displacement= -f.size - vsize});
                                              addTable {dname=(identifierName i); dkind="UnknownValue"; dsize=(string_of_int vsize); dlevel=(string_of_int f.lev); ddispl=(string_of_int (-f.size - vsize)); dvalue=""};
                                              vsize
                                              
  | VarFormalParameter(ix, i, t)       -> let vx = (visitTypeDenoter t) in
                                              ix.run <- UnknownAddress(addressSize, {level=f.lev; displacement= -f.size - addressSize});
                                              addTable {dname=(identifierName i); dkind="UnknownAddress"; dsize=(string_of_int addressSize); dlevel=(string_of_int f.lev); ddispl=(string_of_int (-f.size - addressSize)); dvalue=""};
                                              addressSize
                                          
  | ProcFormalParameter(ix, i, fps)
  | FuncFormalParameter(ix, i, fps, _) -> ix.run <- UnknownRoutine(closureSize, {level=f.lev; displacement= -f.size - closureSize});
                                          addTable {dname=(identifierName i); dkind="UnknownRoutine"; dsize=(string_of_int closureSize); dlevel=(string_of_int f.lev); ddispl=(string_of_int (-f.size - closureSize)); dvalue=""};
                                          closureSize

(* Actual parameters *)
and visitActualParameter ap f = match ap with
    ConstActualParameter(ix, e) -> (visitExpression e f)
    
  | VarActualParameter(ix, v)   -> encodeFetchAddress v f;
                                   addressSize

  | ProcActualParameter(ix, i)
                                    
  | FuncActualParameter(ix, i)  -> (match (obtainDeclAstInfo (obtainIdentifierDeclaration i)).run with
                                      KnownRoutine(s, oa)    -> emit opLOADA 0 (displayRegister f.lev oa.level) 0;
                                                                emit opLOADA 0 rCB oa.displacement
                                                                
                                    | UnknownRoutine(s, oa)  -> emit opLOAD closureSize (displayRegister f.lev oa.level) oa.displacement
                                    
                                    | PrimitiveRoutine(s, d) -> emit opLOADA 0 rSB 0;
                                                                emit opLOADA 0 rPB d
                                    
                                    | _                      -> ());                                    
                                    closureSize

(* Formal parameter sequences *)
and visitFormalParameterSequence fps f = match fps with
    EmptyFormalParameterSequence(ix)              -> 0
  | SingleFormalParameterSequence(ix, fp)         -> (visitFormalParameter fp f)
  | MultipleFormalParameterSequence(ix, fp, fpsx) -> let args1 = (visitFormalParameterSequence fpsx f) in
                                                     let f1 = {lev=f.lev; size=f.size+args1} in
                                                     let args2 = (visitFormalParameter fp f1) in
                                                         args1 + args2

(* Actual parameter sequences *)
and visitActualParameterSequence aps f = match aps with
    EmptyActualParameterSequence(ix)              -> 0
  | SingleActualParameterSequence(ix, ap)         -> (visitActualParameter ap f)
  | MultipleActualParameterSequence(ix, ap, apsx) -> let args1 = (visitActualParameter ap f) in
                                                     let f1 = {lev=f.lev; size=f.size+args1} in
                                                     let args2 = (visitActualParameterSequence apsx f1) in
                                                         args1 + args2

(* Type denoters *)
and visitTypeDenoter t = match t with
    NullTypeDenoter             -> 0
    
  | ErrorTypeDenoter(ix)        -> 0
  
  | AnyTypeDenoter(ix)          -> 0
  
  | SimpleTypeDenoter(ix, i)    -> 0
  
  | ArrayTypeDenoter(ix, il, t) -> if (ix.run == NullRuntimeEntity) then
                                      let esize = (visitTypeDenoter t) in
                                      let tsize = (visitIntegerLiteral il) * esize in
                                      begin
                                        ix.run <- TypeRepresentation(tsize);                                        
                                        tsize
                                      end
                                   else
                                     (obtainRuntimeEntitySize ix.run)
                                     
  | RecordTypeDenoter(ix, ft)   -> if (ix.run == NullRuntimeEntity) then
                                      let tsize = (visitFieldTypeDenoter ft 0) in
                                      begin
                                        ix.run <- TypeRepresentation(tsize);
                                        tsize
                                      end
                                   else
                                      (obtainRuntimeEntitySize ix.run)
  
  | BoolTypeDenoter(ix)         -> if (ix.run == NullRuntimeEntity) then
                                      ix.run <- TypeRepresentation(booleanSize);
                                   booleanSize
                                   
  | IntTypeDenoter(ix)          -> if (ix.run == NullRuntimeEntity) then
                                      ix.run <- TypeRepresentation(integerSize);
                                   integerSize
                                   
  | CharTypeDenoter(ix)         -> if (ix.run == NullRuntimeEntity) then
                                      ix.run <- TypeRepresentation(characterSize);
                                   characterSize
                                      
  
(* Field type denoters *)
and visitFieldTypeDenoter ft offset = match ft with
    SingleFieldTypeDenoter(ix, i, t)        -> if (ix.run == NullRuntimeEntity) then
                                                  let fsize = visitTypeDenoter t in
                                                  begin
                                                     ix.run <- Field(fsize, offset);
                                                     fsize
                                                  end
                                               else
                                                  (obtainRuntimeEntitySize ix.run)

  | MultipleFieldTypeDenoter(ix, i, t, ftx) -> let fsize = 
                                                   if (ix.run == NullRuntimeEntity) then
                                                      let fx = visitTypeDenoter t in
                                                      begin
                                                         ix.run <- Field(fx, offset);
                                                         fx
                                                      end
                                                   else
                                                     (obtainRuntimeEntitySize ix.run)
                                               in
                                                   let offset1 = offset + fsize in
                                                   let recsize = visitFieldTypeDenoter ftx offset1 in
                                                       fsize + recsize
                                                  

(* Integer literals *)
and visitIntegerLiteral il = match il with
    IntegerLiteral(_, s) -> int_of_string s

(* Character literals *)
and visitCharacterLiteral cl = match cl with
    CharacterLiteral(ix, s) -> (int_of_char (String.get s 1))

(* Identifiers *)
and visitIdentifier i f = match i with
    CheckedIdentifier(i, d) -> let entity = (obtainDeclAstInfo !d).run in
                                   (match entity with
                                      KnownRoutine(s,oa)      -> emit opCALL (displayRegister f.lev oa.level) rCB oa.displacement
                                    | UnknownRoutine(s,oa)    -> emit opLOAD closureSize (displayRegister f.lev oa.level) oa.displacement;
                                                                 emit opCALLI 0 0 0
                                    | PrimitiveRoutine(s,d)   -> if (d != idDisplacement) then
                                                                    emit opCALL rSB rPB d
                                    | EqualityRoutine(s,d)    -> emit opLOADL 0 0 (f.size / 2);
                                                                 emit opCALL rSB rPB d
                                    | _                       -> ())
  | _                       -> ()

(* Operators, same as identifiers *)
and visitOperator o f = match o with
  | CheckedOperator(i, d) -> let entity = (obtainDeclAstInfo !d).run in
                                   (match entity with
                                      KnownRoutine(s,oa)      -> emit opCALL (displayRegister f.lev oa.level) rCB oa.displacement
                                    | UnknownRoutine(s,oa)    -> emit opLOAD closureSize (displayRegister f.lev oa.level) oa.displacement;
                                                                 emit opCALLI 0 0 0
                                    | PrimitiveRoutine(s,d)   -> if (d != idDisplacement) then
                                                                    emit opCALL rSB rPB d
                                    | EqualityRoutine(s,d)    -> emit opLOADL 0 0 (f.size / 2);
                                                                 emit opCALL rSB rPB d
                                    | _                       -> ())
  | _                     -> ()


(* Encodes a storing operation *)
and encodeStore v f s =
    let v = (visitVname v f) in
    let entity = (obtainVnameEntity v)
    and valSize = ref s in
        if (s > 255) then
        begin
          ErrorReporter.reportRestriction "can't store values larger than 255 words";
          valSize := 255
        end;
        match entity with
          KnownAddress(s, oa)   -> if (isVnameIndexed v) then
                                   begin
                                      emit opLOADA 0 (displayRegister f.lev oa.level) (oa.displacement + obtainVnameOffset v);
                                      emit opCALL rSB rPB addDisplacement;
                                      emit opSTOREI !valSize 0 0
                                   end
                                   else
                                     emit opSTORE !valSize (displayRegister f.lev oa.level) (oa.displacement + obtainVnameOffset v)
                                     
        | UnknownAddress(s, oa) -> emit opLOAD addressSize (displayRegister f.lev oa.level) oa.displacement;
                                   if (isVnameIndexed v) then
                                      emit opCALL rSB rPB addDisplacement;
                                   if ((obtainVnameOffset v) == 0) then
                                   begin
                                      emit opLOADL 0 0 (obtainVnameOffset v);
                                      emit opCALL rSB rPB addDisplacement
                                   end;
                                   emit opSTOREI !valSize 0 0
                                   
        | _                     -> ()


(* Entodes a fetching operation *)
and encodeFetch v f s =
    let v = (visitVname v f) in
    let entity = (obtainVnameEntity v)
    and valSize = ref s in
        if (s > 255) then
        begin
          ErrorReporter.reportRestriction "can't load values larger than 255 words";
          valSize := 255
        end;
        match entity with
          KnownValue(s, vx)      -> emit opLOADL 0 0 vx
          
        | UnknownValue(s, oa)
        | KnownAddress(s, oa)   -> if (isVnameIndexed v) then
                                   begin
                                      emit opLOADA 0 (displayRegister f.lev oa.level) (oa.displacement + obtainVnameOffset v);
                                      emit opCALL rSB rPB addDisplacement;
                                      emit opLOADI !valSize 0 0
                                   end
                                   else
                                      emit opLOAD !valSize (displayRegister f.lev oa.level) (oa.displacement + obtainVnameOffset v)
                                      
        | UnknownAddress(s, oa) -> emit opLOAD addressSize (displayRegister f.lev oa.level) oa.displacement;
                                   if (isVnameIndexed v) then
                                      emit opCALL rSB rPB addDisplacement;
                                   if ((obtainVnameOffset v) == 0) then
                                   begin
                                      emit opLOADL 0 0 (obtainVnameOffset v);
                                      emit opCALL rSB rPB addDisplacement
                                   end;
                                   emit opLOADI !valSize 0 0
                                   
        | _                     -> ()

(* Encodes an address fetching operation *)
and encodeFetchAddress v f = let vx = (visitVname v f) in match (obtainVnameEntity vx) with
    KnownAddress(s, oa)   -> emit opLOADA 0 (displayRegister f.lev oa.level) (oa.displacement + obtainVnameOffset vx);
                             if (isVnameIndexed vx) then
                                emit opCALL rSB rPB addDisplacement

  | UnknownAddress(s, oa) -> emit opLOAD addressSize (displayRegister f.lev oa.level) oa.displacement;
                             if (isVnameIndexed vx) then
                                emit opCALL rSB rPB addDisplacement;
                             if ((obtainVnameOffset vx) == 0) then
                             begin
                                emit opLOADL 0 0 (obtainVnameOffset vx);
                                emit opCALL rSB rPB addDisplacement
                             end

  | _                     -> ()
                             

(* STANDARD ENVIRONMENT ELABORATION *)

(* Decides run-time representation of a standard constant. *)
let elaborateStdConst decl value = match decl with
    ConstDeclaration(ix,i,e) -> let typeSize = (visitExpression e {lev=0;size=0}) in
                                    ix.run <- KnownValue(typeSize, value)
  | _                        -> ()


(* Decides run-time representation of a standard primitive routine. *)
let elaborateStdPrimRoutine decl offset = 
    (obtainDeclAstInfo decl).run <- PrimitiveRoutine(closureSize, offset)

(* Decides run-time representation of a standard equality routine. *)
let elaborateStdEqRoutine decl offset =
    (obtainDeclAstInfo decl).run <- EqualityRoutine(closureSize, offset)

let _ = (elaborateStdConst (!(retrieve "false")) falseRep);
        (elaborateStdConst (!(retrieve "true")) trueRep);
        (elaborateStdConst (!(retrieve "maxint")) maxintRep);
        (elaborateStdPrimRoutine (!(retrieve "\\")) notDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "/\\")) andDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "\\/")) orDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "+")) addDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "-")) subDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "*")) multDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "/")) divDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "//")) modDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "<")) ltDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "<=")) leDisplacement);
        (elaborateStdPrimRoutine (!(retrieve ">")) gtDisplacement);
        (elaborateStdPrimRoutine (!(retrieve ">=")) geDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "chr")) idDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "ord")) idDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "eol")) eolDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "eof")) eofDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "get")) getDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "put")) putDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "getint")) getintDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "putint")) putintDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "geteol")) geteolDisplacement);
        (elaborateStdPrimRoutine (!(retrieve "puteol")) puteolDisplacement);
        (elaborateStdEqRoutine (!(retrieve "=")) eqDisplacement);
        (elaborateStdEqRoutine (!(retrieve "\\=")) neDisplacement)




(* ENTRYPOINT *)

(* Generates the machine code for the program and saves it in the specified file name *)
let encodeProgram p fname = 
    visitProgram p {lev = 0 ; size = 0};
    emit opHALT 0 0 0;
    try
       let filo = open_out_bin fname in
           for addr = rCB to !nextAddr - 1
           do
              output_binary_int filo codestore.(addr).op;
              output_binary_int filo codestore.(addr).r;
              output_binary_int filo codestore.(addr).n;
              output_binary_int filo codestore.(addr).d
           done;
           close_out filo
    with Sys_error x -> Printf.printf "Couldn't open object file %s.\n" fname