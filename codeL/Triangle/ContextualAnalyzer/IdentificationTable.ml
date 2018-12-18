(* --------------------------------------------- *)
(* Identification Table Module for Caml-Triangle *)
(* Implementation file                           *)
(*                                               *)
(* (c) 2006 Luis Leopoldo Pérez.                 *)
(* Last modification: April 21, 2006             *)
(* --------------------------------------------- *)

open Ast
open RuntimeEntity

type idEntry = {mutable id: string; mutable attr: astDeclaration ref; mutable levl: int}

type idList = NullIdList
            | IdList of idEntry list
            

let level = ref 0

let identifierList = ref NullIdList

let unboxIdList t = match t with
    NullIdList -> []
  | IdList s -> s

let openScope() = incr(level)

let closeScope() =     
    identifierList := IdList(List.filter (fun x -> (x.levl != !level)) (unboxIdList !identifierList));
    decr(level)

let exists newId =
    (List.exists (fun x -> (((String.compare x.id newId) == 0) && (x.levl == !level))) (unboxIdList !identifierList))
    
let enter newId newDecl = 
    let newEntry = {id=newId; attr=newDecl; levl=(!level)} in
    identifierList := IdList([newEntry] @ unboxIdList(!identifierList))      

let retrieve oldId =
    try
       (List.find (fun x -> ((String.compare x.id oldId) == 0)) (unboxIdList !identifierList)).attr
    with Not_found -> ref NullDeclaration
    
let retrieveElement oldId =
    try
       (List.find (fun x -> ((String.compare x.id oldId) == 0) && (x.levl == !level)) (unboxIdList !identifierList))
    with Not_found -> {id="";attr=ref NullDeclaration;levl=0}

    
(* Standard Environment *)

let booleanType = BoolTypeDenoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let charType    = CharTypeDenoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let integerType = IntTypeDenoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let anyType     = AnyTypeDenoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})
let errorType   = ErrorTypeDenoter({pos=Lexing.dummy_pos;run=NullRuntimeEntity})

let booleanDecl = TypeDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Boolean"), booleanType)
let charDecl    = TypeDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Char"), charType)
let integerDecl = TypeDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "Integer"), integerType)

let falseDecl  = ConstDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "false"), CheckedExpression(EmptyExpression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType))
let trueDecl   = ConstDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "true"), CheckedExpression(EmptyExpression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType))
let maxintDecl = ConstDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "maxint"), CheckedExpression(EmptyExpression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), integerType))

let notDecl = UnaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\"), booleanType, booleanType)

let andDecl        = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "/\\"), booleanType, booleanType, booleanType)
let orDecl         = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\/"), booleanType, booleanType, booleanType)
let addDecl        = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "+"), integerType, integerType, integerType)
let substractDecl  = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "-"), integerType, integerType, integerType)
let multiplyDecl   = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "*"), integerType, integerType, integerType)
let divideDecl     = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "/"), integerType, integerType, integerType)
let moduloDecl     = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "//"), integerType, integerType, integerType)
let equalDecl      = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "="), anyType, anyType, booleanType)
let unequalDecl    = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "\\="), anyType, anyType, booleanType)
let lessDecl       = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "<"), integerType, integerType, booleanType)
let notlessDecl    = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ">="), integerType, integerType, booleanType)
let greaterDecl    = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ">"), integerType, integerType, booleanType)
let notgreaterDecl = BinaryOperatorDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Operator({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "<="), integerType, integerType, booleanType)

let getDecl    = ProcDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "get"), SingleFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, VarFormalParameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), charType)), EmptyCommand({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let putDecl    = ProcDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "put"), SingleFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ConstFormalParameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), charType)), EmptyCommand({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let getintDecl = ProcDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "getint"), SingleFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, VarFormalParameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integerType)), EmptyCommand({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let putintDecl = ProcDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "putint"), SingleFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ConstFormalParameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integerType)), EmptyCommand({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let geteolDecl = ProcDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "geteol"), EmptyFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), EmptyCommand({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let puteolDecl = ProcDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "puteol"), EmptyFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), EmptyCommand({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let chrDecl = FuncDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "chr"), SingleFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ConstFormalParameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), integerType)), charType, EmptyExpression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let ordDecl = FuncDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "ord"), SingleFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ConstFormalParameter({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, ""), charType)), integerType, EmptyExpression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let eolDecl = FuncDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "eol"), EmptyFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType, EmptyExpression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))
let eofDecl = FuncDeclaration({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, Identifier({pos=Lexing.dummy_pos;run=NullRuntimeEntity}, "eof"), EmptyFormalParameterSequence({pos=Lexing.dummy_pos;run=NullRuntimeEntity}), booleanType, EmptyExpression({pos=Lexing.dummy_pos;run=NullRuntimeEntity}))

let _ = (enter "Boolean" (ref booleanDecl));
        (enter "Char" (ref charDecl));
        (enter "Integer" (ref integerDecl));
        (enter "false" (ref falseDecl));
        (enter "true" (ref trueDecl));
        (enter "maxint" (ref maxintDecl));
        (enter "\\" (ref notDecl));
        (enter "/\\" (ref andDecl));
        (enter "\\/" (ref orDecl));
        (enter "+" (ref addDecl));
        (enter "-" (ref substractDecl));
        (enter "*" (ref multiplyDecl));
        (enter "/" (ref divideDecl));
        (enter "//" (ref moduloDecl));
        (enter "=" (ref equalDecl));
        (enter "\\=" (ref unequalDecl));
        (enter "<" (ref lessDecl));
        (enter ">=" (ref notlessDecl));
        (enter ">" (ref greaterDecl));
        (enter "<=" (ref notgreaterDecl));
        (enter "get" (ref getDecl));
        (enter "put" (ref putDecl));
        (enter "getint" (ref getintDecl));
        (enter "putint" (ref putintDecl));
        (enter "geteol" (ref geteolDecl));
        (enter "puteol" (ref puteolDecl));
        (enter "chr" (ref chrDecl));
        (enter "ord" (ref ordDecl));
        (enter "eol" (ref eolDecl));
        (enter "eof" (ref eofDecl))
