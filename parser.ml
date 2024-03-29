type token =
  | IDENTIFIER of (string)
  | BCONSTANT of (bool)
  | INTCONSTANT of (int)
  | FST
  | SND
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | EQ
  | GE
  | GT
  | LE
  | LT
  | NE
  | BLAND
  | BLOR
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | AND
  | ARROW
  | FUN
  | IN
  | LET
  | REC
  | TYPE
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Miniml

let primop_of_token = function
  | ADD -> BinOp (BArith BAadd)
  | SUB -> BinOp (BArith BAsub)
  | MUL -> BinOp (BArith BAmul)
  | DIV -> BinOp (BArith BAdiv)
  | MOD -> BinOp (BArith BAmod)
  | EQ -> BinOp (BCompar BCeq)
  | GE -> BinOp (BCompar BCge)
  | GT -> BinOp (BCompar BCgt)
  | LE -> BinOp (BCompar BCle)
  | LT -> BinOp (BCompar BClt)
  | NE -> BinOp (BCompar BCne)
  | _ -> failwith "in primop_of_token: unknown token"

let binary_exp e1 oper e2 = App(PrimOp (primop_of_token oper), Pair(e1, e2))
# 64 "parser.ml"
let yytransl_const = [|
  260 (* FST *);
  261 (* SND *);
  262 (* ADD *);
  263 (* SUB *);
  264 (* MUL *);
  265 (* DIV *);
  266 (* MOD *);
  267 (* EQ *);
  268 (* GE *);
  269 (* GT *);
  270 (* LE *);
  271 (* LT *);
  272 (* NE *);
  273 (* BLAND *);
  274 (* BLOR *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* LBRACE *);
  278 (* RBRACE *);
  279 (* COMMA *);
  280 (* SEMICOLON *);
  281 (* COLON *);
  282 (* QMARK *);
  283 (* IF *);
  284 (* THEN *);
  285 (* ELSE *);
  286 (* WHILE *);
  287 (* FOR *);
  288 (* RETURN *);
  289 (* AND *);
  290 (* ARROW *);
  291 (* FUN *);
  292 (* IN *);
  293 (* LET *);
  294 (* REC *);
  295 (* TYPE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* BCONSTANT *);
  259 (* INTCONSTANT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\005\000\005\000\003\000\003\000\006\000\006\000\
\006\000\006\000\008\000\008\000\008\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\010\000\010\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\007\000\004\000\004\000\
\004\000\004\000\014\000\013\000\013\000\000\000"

let yylen = "\002\000\
\001\000\004\000\000\000\002\000\000\000\002\000\001\000\003\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\002\000\001\000\001\000\006\000\
\004\000\005\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\038\000\001\000\000\000\006\000\023\000\
\024\000\025\000\021\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\000\000\000\000\026\000\000\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\027\000\000\000\000\000\000\000\000\000\000\000\
\004\000\002\000\008\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\000\000\000\000\035\000\034\000\037\000\032\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\017\000\030\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000\047\000\048\000"

let yysindex = "\255\255\
\243\254\000\000\027\255\000\000\000\000\003\255\000\000\000\000\
\000\000\000\000\000\000\000\000\003\255\003\255\038\255\249\254\
\017\255\011\255\000\000\030\255\035\255\000\000\013\255\000\000\
\022\255\016\255\018\255\054\255\042\255\067\000\013\255\013\255\
\013\255\013\255\013\255\013\255\013\255\013\255\013\255\013\255\
\013\255\000\000\000\000\003\255\003\255\057\255\033\255\037\255\
\000\000\000\000\000\000\000\000\000\000\011\255\011\255\011\255\
\011\255\011\255\011\255\011\255\011\255\043\255\000\000\003\255\
\003\255\054\255\003\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\008\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\071\000\029\000\000\000\111\000\083\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\255\
\000\000\000\000\000\000\000\000\000\000\057\000\085\000\137\000\
\163\000\189\000\215\000\241\000\018\001\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\245\255\000\000\024\000\248\255\000\000\
\000\000\000\000\000\000\050\000\008\000\000\000"

let yytablesize = 566
let yytable = "\001\000\
\028\000\025\000\026\000\008\000\009\000\010\000\011\000\012\000\
\005\000\005\000\005\000\005\000\005\000\008\000\009\000\010\000\
\011\000\012\000\031\000\032\000\033\000\013\000\051\000\052\000\
\053\000\003\000\005\000\007\000\011\000\014\000\028\000\013\000\
\062\000\063\000\005\000\034\000\035\000\015\000\027\000\016\000\
\029\000\043\000\005\000\044\000\005\000\036\000\037\000\038\000\
\039\000\040\000\041\000\045\000\068\000\069\000\046\000\071\000\
\012\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\049\000\050\000\064\000\065\000\066\000\003\000\067\000\
\042\000\070\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\020\000\000\000\000\000\028\000\000\000\000\000\000\000\
\028\000\000\000\000\000\000\000\028\000\028\000\000\000\000\000\
\000\000\028\000\011\000\011\000\028\000\000\000\000\000\011\000\
\011\000\011\000\011\000\011\000\011\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\011\000\011\000\000\000\000\000\000\000\011\000\012\000\012\000\
\011\000\000\000\000\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\012\000\012\000\000\000\000\000\
\000\000\012\000\013\000\013\000\012\000\000\000\000\000\013\000\
\013\000\013\000\013\000\013\000\013\000\000\000\031\000\000\000\
\013\000\000\000\031\000\000\000\013\000\000\000\031\000\031\000\
\013\000\013\000\000\000\031\000\000\000\013\000\031\000\000\000\
\013\000\014\000\014\000\014\000\014\000\014\000\014\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\014\000\000\000\
\000\000\000\000\014\000\014\000\000\000\000\000\000\000\014\000\
\000\000\000\000\014\000\017\000\017\000\017\000\017\000\017\000\
\017\000\000\000\000\000\000\000\017\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\017\000\017\000\000\000\000\000\
\000\000\017\000\000\000\000\000\017\000\018\000\018\000\018\000\
\018\000\018\000\018\000\000\000\000\000\000\000\018\000\000\000\
\000\000\000\000\018\000\000\000\000\000\000\000\018\000\018\000\
\000\000\000\000\000\000\018\000\000\000\000\000\018\000\015\000\
\015\000\015\000\015\000\015\000\015\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
\015\000\015\000\000\000\000\000\000\000\015\000\000\000\000\000\
\015\000\019\000\019\000\019\000\019\000\019\000\019\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\019\000\019\000\000\000\000\000\000\000\019\000\
\000\000\000\000\019\000\016\000\016\000\016\000\016\000\016\000\
\016\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\
\016\000\000\000\000\000\000\000\016\000\016\000\000\000\000\000\
\000\000\016\000\000\000\000\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\020\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\020\000\020\000\000\000\
\000\000\000\000\020\000\000\000\000\000\020\000"

let yycheck = "\001\000\
\000\000\013\000\014\000\001\001\002\001\003\001\004\001\005\001\
\001\001\002\001\003\001\004\001\005\001\001\001\002\001\003\001\
\004\001\005\001\008\001\009\001\010\001\019\001\031\000\032\000\
\033\000\039\001\019\001\001\001\000\000\027\001\038\001\019\001\
\044\000\045\000\027\001\006\001\007\001\035\001\001\001\037\001\
\024\001\020\001\035\001\028\001\037\001\011\001\012\001\013\001\
\014\001\015\001\016\001\034\001\064\000\065\000\001\001\067\000\
\000\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\024\001\000\000\011\001\036\001\033\001\000\000\029\001\
\023\000\066\000\036\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\000\000\255\255\255\255\020\001\255\255\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\029\001\255\255\255\255\
\255\255\033\001\006\001\007\001\036\001\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\255\255\
\020\001\255\255\255\255\255\255\024\001\255\255\255\255\255\255\
\028\001\029\001\255\255\255\255\255\255\033\001\006\001\007\001\
\036\001\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\020\001\255\255\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\029\001\255\255\255\255\
\255\255\033\001\006\001\007\001\036\001\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\020\001\255\255\
\020\001\255\255\024\001\255\255\024\001\255\255\028\001\029\001\
\028\001\029\001\255\255\033\001\255\255\033\001\036\001\255\255\
\036\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\255\255\255\255\020\001\255\255\255\255\255\255\024\001\255\255\
\255\255\255\255\028\001\029\001\255\255\255\255\255\255\033\001\
\255\255\255\255\036\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\020\001\255\255\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\029\001\255\255\255\255\
\255\255\033\001\255\255\255\255\036\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\255\255\020\001\255\255\
\255\255\255\255\024\001\255\255\255\255\255\255\028\001\029\001\
\255\255\255\255\255\255\033\001\255\255\255\255\036\001\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\255\255\
\020\001\255\255\255\255\255\255\024\001\255\255\255\255\255\255\
\028\001\029\001\255\255\255\255\255\255\033\001\255\255\255\255\
\036\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\255\255\255\255\020\001\255\255\255\255\255\255\024\001\255\255\
\255\255\255\255\028\001\029\001\255\255\255\255\255\255\033\001\
\255\255\255\255\036\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\020\001\255\255\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\029\001\255\255\255\255\
\255\255\033\001\255\255\255\255\036\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\255\255\255\255\020\001\255\255\255\255\
\255\255\024\001\255\255\255\255\255\255\028\001\029\001\255\255\
\255\255\255\255\033\001\255\255\255\255\036\001"

let yynames_const = "\
  FST\000\
  SND\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  MOD\000\
  EQ\000\
  GE\000\
  GT\000\
  LE\000\
  LT\000\
  NE\000\
  BLAND\000\
  BLOR\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  SEMICOLON\000\
  COLON\000\
  QMARK\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  AND\000\
  ARROW\000\
  FUN\000\
  IN\000\
  LET\000\
  REC\000\
  TYPE\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  BCONSTANT\000\
  INTCONSTANT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 41 "parser.mly"
            ( _1 )
# 366 "parser.ml"
               : Miniml.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typedef) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'mlexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'end_marker_opt) in
    Obj.repr(
# 46 "parser.mly"
  (Prog (_1, _2) )
# 375 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
  ( )
# 381 "parser.ml"
               : 'end_marker_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
  ( )
# 387 "parser.ml"
               : 'end_marker_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
     ( None )
# 393 "parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
  (  Some (_2) )
# 400 "parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list_as_mlexp) in
    Obj.repr(
# 66 "parser.mly"
    ( _1 )
# 407 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list_as_mlexp) in
    Obj.repr(
# 68 "parser.mly"
     ( binary_exp _1 MUL _3 )
# 415 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list_as_mlexp) in
    Obj.repr(
# 70 "parser.mly"
     ( binary_exp _1 DIV _3 )
# 423 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list_as_mlexp) in
    Obj.repr(
# 72 "parser.mly"
     ( binary_exp _1 MOD _3 )
# 431 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 77 "parser.mly"
    ( _1 )
# 438 "parser.ml"
               : 'add_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 79 "parser.mly"
     ( binary_exp _1 ADD _3 )
# 446 "parser.ml"
               : 'add_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 81 "parser.mly"
     ( binary_exp _1 SUB _3 )
# 454 "parser.ml"
               : 'add_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_exp) in
    Obj.repr(
# 86 "parser.mly"
    ( _1 )
# 461 "parser.ml"
               : 'compare_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 88 "parser.mly"
     ( binary_exp _1 GT _3 )
# 469 "parser.ml"
               : 'compare_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 90 "parser.mly"
     ( binary_exp _1 LT _3 )
# 477 "parser.ml"
               : 'compare_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 92 "parser.mly"
     ( binary_exp _1 EQ _3 )
# 485 "parser.ml"
               : 'compare_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 94 "parser.mly"
     ( binary_exp _1 GE _3 )
# 493 "parser.ml"
               : 'compare_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 96 "parser.mly"
     ( binary_exp _1 LE _3 )
# 501 "parser.ml"
               : 'compare_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 98 "parser.mly"
     ( binary_exp _1 NE _3 )
# 509 "parser.ml"
               : 'compare_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
  ( Fst)
# 515 "parser.ml"
               : 'unary_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
  ( Snd)
# 521 "parser.ml"
               : 'unary_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
    ( Var(_1) )
# 528 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 112 "parser.mly"
     ( Bool(_1) )
# 535 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 114 "parser.mly"
     ( Int(_1) )
# 542 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_op) in
    Obj.repr(
# 117 "parser.mly"
     ( PrimOp (UnOp(_1)) )
# 549 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mlexp) in
    Obj.repr(
# 119 "parser.mly"
     ( _2 )
# 556 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp) in
    Obj.repr(
# 124 "parser.mly"
  ( [_1] )
# 563 "parser.ml"
               : 'primary_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'primary_exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list) in
    Obj.repr(
# 126 "parser.mly"
    ( _1 :: _2 )
# 571 "parser.ml"
               : 'primary_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list) in
    Obj.repr(
# 131 "parser.mly"
      ( match _1 with
      | [e] -> e
      | e::r -> List.fold_left (fun x y -> App(x, y)) e r
      |  _ -> failwith "parser: empty list impossible"
      )
# 582 "parser.ml"
               : 'primary_exp_list_as_mlexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compare_exp) in
    Obj.repr(
# 140 "parser.mly"
    ( _1 )
# 589 "parser.ml"
               : 'mlexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'mlexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'mlexp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'mlexp) in
    Obj.repr(
# 142 "parser.mly"
    ( Cond(_2, _4, _6) )
# 598 "parser.ml"
               : 'mlexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'mlexp) in
    Obj.repr(
# 144 "parser.mly"
    ( Fn (_2, _4) )
# 606 "parser.ml"
               : 'mlexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'let_binding_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'mlexp) in
    Obj.repr(
# 147 "parser.mly"
    ( Fix (_3, _5) )
# 614 "parser.ml"
               : 'mlexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mlexp) in
    Obj.repr(
# 152 "parser.mly"
  ( (_1, _3) )
# 622 "parser.ml"
               : 'let_binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding) in
    Obj.repr(
# 157 "parser.mly"
  ( [_1] )
# 629 "parser.ml"
               : 'let_binding_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'let_binding) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'let_binding_list) in
    Obj.repr(
# 159 "parser.mly"
    ( _1 :: _3 )
# 637 "parser.ml"
               : 'let_binding_list))
(* Entry start *)
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
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Miniml.prog)
