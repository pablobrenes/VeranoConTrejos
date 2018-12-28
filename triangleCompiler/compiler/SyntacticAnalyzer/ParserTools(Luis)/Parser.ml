open Token;;
open Parsing;;
let _ = parse_error;;
# 2 "Parser.mly"

(* --------------------------------------------------- *)
(* Parser generator file (ocamlyacc) for Caml-Triangle *)
(*                                                     *)
(* (c) 2006 Luis Leopoldo Pérez.                       *)
(* Last modification: April 12, 2006                   *)
(* --------------------------------------------------- *)

      open Ast
      open Parsing
      open ErrorReporter
      open RuntimeEntity
      
      let parse_error s = ()
# 55 "Parser.ml"
let yytransl_const = [|
  261 (* ARRAY *);
  262 (* BEGIN *);
  263 (* CONST *);
  264 (* DO *);
  265 (* ELSE *);
  266 (* END *);
  267 (* FUNC *);
  268 (* IF *);
  269 (* IN *);
  270 (* LET *);
  271 (* OF *);
  272 (* PROC *);
  273 (* RECORD *);
  274 (* THEN *);
  275 (* TYPE *);
  276 (* VAR *);
  277 (* WHILE *);
  278 (* DOT *);
  279 (* COLON *);
  280 (* SEMICOLON *);
  281 (* COMMA *);
  282 (* BECOMES *);
  283 (* IS *);
  284 (* LPAREN *);
  285 (* RPAREN *);
  286 (* LBRACKET *);
  287 (* RBRACKET *);
  288 (* LCURLY *);
  289 (* RCURLY *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INTLITERAL *);
  258 (* CHARLITERAL *);
  259 (* IDENTIFIER *);
  260 (* OPERATOR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\014\000\014\000\015\000\015\000\004\000\004\000\004\000\
\008\000\008\000\008\000\016\000\016\000\016\000\016\000\016\000\
\018\000\018\000\019\000\019\000\020\000\020\000\020\000\020\000\
\007\000\007\000\021\000\021\000\022\000\022\000\022\000\022\000\
\017\000\017\000\017\000\023\000\023\000\012\000\013\000\006\000\
\011\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\000\000\003\000\004\000\003\000\
\004\000\006\000\004\000\001\000\006\000\004\000\001\000\001\000\
\003\000\001\000\001\000\001\000\004\000\002\000\003\000\003\000\
\003\000\003\000\005\000\001\000\003\000\001\000\003\000\004\000\
\001\000\003\000\001\000\004\000\004\000\007\000\009\000\004\000\
\000\000\001\000\001\000\003\000\003\000\004\000\005\000\007\000\
\000\000\001\000\001\000\003\000\001\000\002\000\002\000\002\000\
\001\000\004\000\003\000\003\000\005\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\064\000\000\000\000\000\000\000\000\000\
\066\000\000\000\003\000\000\000\000\000\000\000\015\000\062\000\
\063\000\065\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\018\000\019\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\031\000\006\000\000\000\000\000\000\000\
\000\000\053\000\000\000\050\000\000\000\000\000\000\000\023\000\
\000\000\025\000\000\000\024\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\009\000\034\000\011\000\032\000\
\056\000\055\000\000\000\030\000\007\000\000\000\000\000\014\000\
\029\000\000\000\000\000\021\000\036\000\000\000\000\000\000\000\
\000\000\000\000\042\000\000\000\000\000\000\000\000\000\057\000\
\040\000\037\000\052\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\027\000\000\000\000\000\000\000\045\000\000\000\044\000\
\000\000\000\000\000\000\059\000\000\000\000\000\046\000\000\000\
\038\000\058\000\000\000\000\000\047\000\000\000\000\000\000\000\
\039\000\061\000\048\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\024\000\074\000\026\000\075\000\038\000\
\027\000\028\000\029\000\030\000\031\000\054\000\052\000\039\000\
\121\000\114\000\115\000\116\000\076\000\077\000\136\000"

let yysindex = "\013\000\
\192\255\000\000\000\000\000\000\109\255\179\255\065\255\179\255\
\000\000\004\000\000\000\038\255\244\254\253\254\000\000\000\000\
\000\000\000\000\179\255\065\255\179\255\179\255\042\255\017\255\
\049\255\041\255\070\255\000\000\021\255\000\000\000\000\000\000\
\042\255\042\255\042\255\042\255\042\255\249\254\000\000\074\255\
\109\255\000\000\042\255\179\255\179\255\157\255\000\000\068\255\
\013\255\054\255\062\255\057\255\067\255\069\255\109\255\157\255\
\021\255\000\000\080\255\073\255\082\255\084\255\090\255\109\255\
\183\255\109\255\000\000\000\000\000\000\085\255\042\255\042\255\
\042\255\000\000\095\255\000\000\100\255\179\255\179\255\000\000\
\179\255\000\000\179\255\000\000\118\255\099\255\000\000\179\255\
\255\254\255\254\072\255\072\255\000\000\000\000\000\000\000\000\
\000\000\000\000\017\255\000\000\000\000\157\255\124\255\000\000\
\000\000\110\255\109\255\000\000\000\000\042\255\042\255\042\255\
\111\255\114\255\000\000\115\255\122\255\143\255\042\255\000\000\
\000\000\000\000\000\000\179\255\042\255\000\000\127\255\128\255\
\139\255\072\255\140\255\255\254\137\255\150\255\151\255\165\255\
\000\000\000\000\255\254\255\254\072\255\000\000\072\255\000\000\
\109\255\072\255\072\255\000\000\147\255\149\255\000\000\159\255\
\000\000\000\000\163\255\161\255\000\000\179\255\042\255\072\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\011\000\000\000\000\000\000\000\003\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\119\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\001\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\167\255\000\000\000\000\
\000\000\000\000\166\255\000\000\000\000\000\000\191\255\167\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\172\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\176\255\176\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\255\000\000\000\000\000\000\000\000\000\000\
\000\000\181\255\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\186\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\176\255\176\255\000\000\000\000\000\000\000\000\
\022\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\198\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\207\000\253\255\065\000\012\000\007\000\160\000\197\000\
\000\000\227\255\191\000\101\000\000\000\095\000\140\000\157\000\
\218\255\175\255\091\000\000\000\122\000\000\000\066\000"

let yytablesize = 344
let yytable = "\058\000\
\030\000\004\000\005\000\042\000\005\000\064\000\047\000\013\000\
\117\000\110\000\005\000\013\000\005\000\001\000\111\000\046\000\
\065\000\025\000\112\000\040\000\041\000\016\000\017\000\004\000\
\018\000\079\000\005\000\087\000\020\000\053\000\048\000\054\000\
\050\000\051\000\005\000\054\000\065\000\067\000\043\000\059\000\
\060\000\061\000\062\000\063\000\004\000\005\000\045\000\013\000\
\021\000\068\000\022\000\085\000\023\000\122\000\012\000\069\000\
\070\000\149\000\150\000\043\000\093\000\013\000\095\000\044\000\
\032\000\012\000\055\000\045\000\056\000\012\000\013\000\033\000\
\013\000\018\000\004\000\034\000\118\000\097\000\098\000\100\000\
\035\000\066\000\080\000\036\000\037\000\078\000\081\000\082\000\
\119\000\103\000\104\000\142\000\051\000\083\000\106\000\113\000\
\113\000\120\000\120\000\109\000\089\000\084\000\151\000\126\000\
\152\000\012\000\088\000\154\000\155\000\090\000\091\000\004\000\
\092\000\013\000\005\000\096\000\127\000\128\000\129\000\012\000\
\006\000\163\000\007\000\101\000\102\000\135\000\107\000\108\000\
\012\000\008\000\012\000\053\000\124\000\130\000\125\000\137\000\
\120\000\099\000\113\000\132\000\030\000\153\000\131\000\016\000\
\030\000\113\000\113\000\120\000\030\000\120\000\133\000\013\000\
\120\000\120\000\139\000\140\000\015\000\016\000\017\000\004\000\
\018\000\141\000\143\000\145\000\146\000\135\000\120\000\071\000\
\019\000\161\000\020\000\012\000\072\000\147\000\148\000\156\000\
\073\000\157\000\015\000\016\000\017\000\004\000\018\000\160\000\
\021\000\158\000\022\000\159\000\023\000\033\000\019\000\003\000\
\020\000\034\000\004\000\049\000\028\000\005\000\035\000\005\000\
\051\000\036\000\037\000\006\000\041\000\007\000\021\000\060\000\
\022\000\012\000\023\000\014\000\008\000\026\000\043\000\086\000\
\049\000\057\000\134\000\138\000\105\000\094\000\144\000\123\000\
\162\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\030\000\030\000\030\000\005\000\005\000\030\000\005\000\005\000\
\000\000\000\000\030\000\000\000\000\000\000\000\030\000\000\000\
\030\000\030\000\005\000\041\000\005\000\030\000\030\000\030\000\
\020\000\030\000\005\000\000\000\020\000\020\000\020\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\000\000\000\000\020\000\020\000\000\000\000\000\
\000\000\020\000\000\000\020\000\000\000\020\000\012\000\012\000\
\012\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\012\000\012\000\
\000\000\000\000\000\000\012\000\000\000\012\000\000\000\012\000"

let yycheck = "\029\000\
\000\000\003\001\000\000\000\000\000\000\013\001\010\001\001\000\
\090\000\011\001\000\000\005\000\010\001\001\000\016\001\028\001\
\024\001\006\000\020\001\008\000\024\001\001\001\002\001\003\001\
\004\001\013\001\024\001\057\000\000\000\023\000\019\000\025\001\
\021\000\022\000\013\001\029\001\024\001\041\000\022\001\033\000\
\034\000\035\000\036\000\037\000\003\001\024\001\030\001\041\000\
\028\001\043\000\030\001\055\000\032\001\092\000\000\000\044\000\
\045\000\139\000\140\000\022\001\064\000\055\000\066\000\026\001\
\000\001\001\000\018\001\030\001\028\001\005\000\064\000\007\001\
\066\000\004\001\003\001\011\001\005\001\071\000\072\000\073\000\
\016\001\008\001\029\001\019\001\020\001\018\001\025\001\031\001\
\017\001\078\000\079\000\130\000\081\000\027\001\083\000\089\000\
\090\000\091\000\092\000\088\000\028\001\033\001\141\000\107\000\
\143\000\041\000\027\001\146\000\147\000\028\001\027\001\003\001\
\023\001\107\000\006\001\031\001\110\000\111\000\112\000\055\000\
\012\001\160\000\014\001\029\001\025\001\119\000\009\001\029\001\
\064\000\021\001\066\000\125\000\009\001\023\001\025\001\124\000\
\130\000\073\000\132\000\025\001\022\001\145\000\029\001\001\001\
\026\001\139\000\140\000\141\000\030\001\143\000\029\001\145\000\
\146\000\147\000\028\001\028\001\000\001\001\001\002\001\003\001\
\004\001\023\001\023\001\027\001\015\001\159\000\160\000\011\001\
\012\001\158\000\014\001\107\000\016\001\023\001\010\001\029\001\
\020\001\029\001\000\001\001\001\002\001\003\001\004\001\023\001\
\028\001\027\001\030\001\025\001\032\001\007\001\012\001\000\001\
\014\001\011\001\003\001\029\001\031\001\006\001\016\001\009\001\
\029\001\019\001\020\001\012\001\029\001\014\001\028\001\010\001\
\030\001\145\000\032\001\005\000\021\001\033\001\029\001\056\000\
\020\000\027\000\118\000\125\000\081\000\065\000\132\000\102\000\
\159\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\255\255\255\255\
\008\001\009\001\010\001\009\001\010\001\013\001\010\001\013\001\
\255\255\255\255\018\001\255\255\255\255\255\255\022\001\255\255\
\024\001\025\001\024\001\024\001\024\001\029\001\030\001\031\001\
\004\001\033\001\024\001\255\255\008\001\009\001\010\001\255\255\
\255\255\013\001\255\255\255\255\255\255\255\255\018\001\255\255\
\255\255\255\255\255\255\255\255\024\001\025\001\255\255\255\255\
\255\255\029\001\255\255\031\001\255\255\033\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\255\255\255\255\255\255\
\018\001\255\255\255\255\255\255\255\255\255\255\024\001\025\001\
\255\255\255\255\255\255\029\001\255\255\031\001\255\255\033\001"

let yynames_const = "\
  ARRAY\000\
  BEGIN\000\
  CONST\000\
  DO\000\
  ELSE\000\
  END\000\
  FUNC\000\
  IF\000\
  IN\000\
  LET\000\
  OF\000\
  PROC\000\
  RECORD\000\
  THEN\000\
  TYPE\000\
  VAR\000\
  WHILE\000\
  DOT\000\
  COLON\000\
  SEMICOLON\000\
  COMMA\000\
  BECOMES\000\
  IS\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LCURLY\000\
  RCURLY\000\
  EOF\000\
  "

let yynames_block = "\
  INTLITERAL\000\
  CHARLITERAL\000\
  IDENTIFIER\000\
  OPERATOR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    Obj.repr(
# 39 "Parser.mly"
                            ( Program({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 335 "Parser.ml"
               : Ast.astProgram))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "Parser.mly"
                            ( ErrorReporter.reportError "Command expected here." (rhs_start_pos(1)); 
                              raise Parse_error )
# 342 "Parser.ml"
               : Ast.astProgram))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 46 "Parser.mly"
                                          ( _1 )
# 349 "Parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 47 "Parser.mly"
                                          ( SequentialCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 357 "Parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "Parser.mly"
                                                                      ( EmptyCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}) )
# 363 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 51 "Parser.mly"
                                                                      ( AssignCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 371 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Actual_Parameter_Sequence) in
    Obj.repr(
# 52 "Parser.mly"
                                                                      ( CallCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 379 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Command) in
    Obj.repr(
# 53 "Parser.mly"
                                                                      ( _2 )
# 386 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Declaration) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 54 "Parser.mly"
                                                                      ( LetCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 394 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'single_Command) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 55 "Parser.mly"
                                                                      ( IfCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _6) )
# 403 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 56 "Parser.mly"
                                                                      ( WhileCommand({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 411 "Parser.ml"
               : 'single_Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'secondary_Expression) in
    Obj.repr(
# 61 "Parser.mly"
                                                             ( _1 )
# 418 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 62 "Parser.mly"
                                                             ( IfExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _6) )
# 427 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Declaration) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 63 "Parser.mly"
                                                             ( LetExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 435 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "Parser.mly"
                                                             ( ErrorReporter.reportError "Expression expected here." (rhs_start_pos(1)); 
                                                               raise Parse_error )
# 442 "Parser.ml"
               : 'Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 69 "Parser.mly"
                                                                       ( _1 )
# 449 "Parser.ml"
               : 'secondary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'secondary_Expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Operator) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 70 "Parser.mly"
                                                                       ( BinaryExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _2, _3) )
# 458 "Parser.ml"
               : 'secondary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Integer_Literal) in
    Obj.repr(
# 73 "Parser.mly"
                                                                       ( IntegerExpression({pos=rhs_start_pos(1); run=NullRuntimeEntity}, _1) )
# 465 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Character_Literal) in
    Obj.repr(
# 74 "Parser.mly"
                                                                       ( CharacterExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity},_1) )
# 472 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Vname) in
    Obj.repr(
# 75 "Parser.mly"
                                                                       ( VnameExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 479 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Actual_Parameter_Sequence) in
    Obj.repr(
# 76 "Parser.mly"
                                                                       ( CallExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 487 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Operator) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_Expression) in
    Obj.repr(
# 77 "Parser.mly"
                                                                       ( UnaryExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _2) )
# 495 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expression) in
    Obj.repr(
# 78 "Parser.mly"
                                                                       ( _2 )
# 502 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Record_Aggregate) in
    Obj.repr(
# 79 "Parser.mly"
                                                                       ( RecordExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 509 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Array_Aggregate) in
    Obj.repr(
# 80 "Parser.mly"
                                                                       ( ArrayExpression({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 516 "Parser.ml"
               : 'primary_Expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 84 "Parser.mly"
                                                                  ( SingleRecordAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 524 "Parser.ml"
               : 'Record_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Record_Aggregate) in
    Obj.repr(
# 85 "Parser.mly"
                                                                  ( MultipleRecordAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3, _5) )
# 533 "Parser.ml"
               : 'Record_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 89 "Parser.mly"
                                                  ( SingleArrayAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 540 "Parser.ml"
               : 'Array_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Array_Aggregate) in
    Obj.repr(
# 90 "Parser.mly"
                                                  ( MultipleArrayAggregate({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 548 "Parser.ml"
               : 'Array_Aggregate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Identifier) in
    Obj.repr(
# 94 "Parser.mly"
                                          ( SimpleVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 555 "Parser.ml"
               : 'Vname))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Identifier) in
    Obj.repr(
# 95 "Parser.mly"
                                          ( DotVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 563 "Parser.ml"
               : 'Vname))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Expression) in
    Obj.repr(
# 96 "Parser.mly"
                                          ( SubscriptVname({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 571 "Parser.ml"
               : 'Vname))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'single_Declaration) in
    Obj.repr(
# 100 "Parser.mly"
                                                      ( _1 )
# 578 "Parser.ml"
               : 'Declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Declaration) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'single_Declaration) in
    Obj.repr(
# 101 "Parser.mly"
                                                      ( SequentialDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 586 "Parser.ml"
               : 'Declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "Parser.mly"
                                                      ( ErrorReporter.reportError "Declaration expected here." (rhs_start_pos(1)); 
                                                        raise Parse_error )
# 593 "Parser.ml"
               : 'Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 106 "Parser.mly"
                                                                                                             ( ConstDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 601 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 107 "Parser.mly"
                                                                                                             ( VarDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 609 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'Formal_Parameter_Sequence) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'single_Command) in
    Obj.repr(
# 108 "Parser.mly"
                                                                                                             ( ProcDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _7) )
# 618 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'Formal_Parameter_Sequence) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'Type_denoter) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 109 "Parser.mly"
                                                                                                             ( FuncDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _7, _9) )
# 628 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 110 "Parser.mly"
                                                                                                             ( TypeDeclaration({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 636 "Parser.ml"
               : 'single_Declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "Parser.mly"
                                                            ( EmptyFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}) )
# 642 "Parser.ml"
               : 'Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Formal_Parameter_Sequence) in
    Obj.repr(
# 115 "Parser.mly"
                                                            ( _1 )
# 649 "Parser.ml"
               : 'Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Formal_Parameter) in
    Obj.repr(
# 118 "Parser.mly"
                                                                                          ( SingleFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity},_1) )
# 656 "Parser.ml"
               : 'proper_Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Formal_Parameter) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Formal_Parameter_Sequence) in
    Obj.repr(
# 119 "Parser.mly"
                                                                                          ( MultipleFormalParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 664 "Parser.ml"
               : 'proper_Formal_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 122 "Parser.mly"
                                                                                             ( ConstFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 672 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 123 "Parser.mly"
                                                                                             ( VarFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 680 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Formal_Parameter_Sequence) in
    Obj.repr(
# 124 "Parser.mly"
                                                                                             ( ProcFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 688 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'Identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'Formal_Parameter_Sequence) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 125 "Parser.mly"
                                                                                             ( FuncFormalParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4, _7) )
# 697 "Parser.ml"
               : 'Formal_Parameter))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "Parser.mly"
                                                            ( EmptyActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}) )
# 703 "Parser.ml"
               : 'Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Actual_Parameter_Sequence) in
    Obj.repr(
# 130 "Parser.mly"
                                                            ( _1 )
# 710 "Parser.ml"
               : 'Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Actual_Parameter) in
    Obj.repr(
# 133 "Parser.mly"
                                                                                          ( SingleActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 717 "Parser.ml"
               : 'proper_Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Actual_Parameter) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'proper_Actual_Parameter_Sequence) in
    Obj.repr(
# 134 "Parser.mly"
                                                                                          ( MultipleActualParameterSequence({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 725 "Parser.ml"
               : 'proper_Actual_Parameter_Sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Expression) in
    Obj.repr(
# 137 "Parser.mly"
                                  ( ConstActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 732 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Vname) in
    Obj.repr(
# 138 "Parser.mly"
                                  ( VarActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 739 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Identifier) in
    Obj.repr(
# 139 "Parser.mly"
                                  ( ProcActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 746 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Identifier) in
    Obj.repr(
# 140 "Parser.mly"
                                  ( FuncActualParameter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 753 "Parser.ml"
               : 'Actual_Parameter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Identifier) in
    Obj.repr(
# 144 "Parser.mly"
                                                    ( SimpleTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 760 "Parser.ml"
               : 'Type_denoter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Integer_Literal) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 145 "Parser.mly"
                                                    ( ArrayTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2, _4) )
# 768 "Parser.ml"
               : 'Type_denoter))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Record_Type_denoter) in
    Obj.repr(
# 146 "Parser.mly"
                                                    ( RecordTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _2) )
# 775 "Parser.ml"
               : 'Type_denoter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type_denoter) in
    Obj.repr(
# 149 "Parser.mly"
                                                                             ( SingleFieldTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3) )
# 783 "Parser.ml"
               : 'Record_Type_denoter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Type_denoter) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Record_Type_denoter) in
    Obj.repr(
# 150 "Parser.mly"
                                                                             ( MultipleFieldTypeDenoter({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1, _3, _5) )
# 792 "Parser.ml"
               : 'Record_Type_denoter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 154 "Parser.mly"
                            ( IntegerLiteral({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 799 "Parser.ml"
               : 'Integer_Literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "Parser.mly"
                               ( CharacterLiteral({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 806 "Parser.ml"
               : 'Character_Literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 162 "Parser.mly"
                       ( Identifier({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 813 "Parser.ml"
               : 'Identifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "Parser.mly"
                   ( Operator({pos=rhs_start_pos(1);run=NullRuntimeEntity}, _1) )
# 820 "Parser.ml"
               : 'Operator))
(* Entry parseProgram *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parseProgram (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.astProgram)
;;
