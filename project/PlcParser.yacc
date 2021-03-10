%%

%name PlcParser

%pos int

%term SEMICOL | COLON | DOUBCOLON | COMMA
    | VAR
    | EQ | INEQ | LESS | LESSEQ
    | FUN | REC | FN
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT
    | MINUS | PLUS | MULTI | DIV
    | HD | TL
    | ISE
    | PRINT
    | AND
    | LBRACE | RBRACE
    | LBRACKET | RBRACKET
    | LPARENT | RPARENT
    | ARROW | DOUBARROW
    | END
    | TRUE of bool | FALSE of bool 
    | PIPE
    | NIL | BOOL | INT
    | UNDERSCORE
    | NAME of string
    | CINT of int
    | EOF
    
%nonterm Prog of expr
    | Decl of expr
    | Expr of expr
    | AtomicExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of list
    | CondExpr of expr option
    | Args of plcType
    | Params of plcType
    | TypedVar of plcType * string
    | Type of plcType
    | AtomicType of plcType
    | Types of plcType list
    | Nat of expr

%right SEMICOL ARROW DOUBCOLON
%left ELSE AND EQ INEQ LESS LESSEQ PLUS MINUS MULTI DIV RBRACKET
%nonassoc IF NOT HD TL ISE PRINT

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
| Decl (Decl)

Decl : VAR NAME EQ Expr SEMICOL Prog (Let(NAME, Expr, Prog))
| FUN NAME Args EQ Expr SEMICOL Prog (Let(NAME, Anon(Args, $list, Expr), Prog))
| FUN REC NAME Args COLON Type EQ Expr SEMICOL Prog (Letrec(NAME, Args, Type, Expr, Prog)) 

Expr : AtomicExpr (AtomicExpr)
| AppExpr (AppExpr)
| IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
| MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
| NOT Expr (Prim1("!", Expr))
| MINUS Expr (Prim1("-", Expr))
| HD Expr (Prim1("hd", Expr))
| TL Expr (Prim1("tl", Expr))
| ISE Expr (Prim1("ise", Expr))
| PRINT Expr (Prim1("print", Expr))
| Expr AND Expr (Prim2("&&", Expr1, Expr2))
| Expr PLUS Expr (Prim2("+", Expr1, Expr2))
| Expr MINUS Expr (Prim2("-", Expr1, Expr2))
| Expr MULTI Expr (Prim2("*", Expr1, Expr2))
| Expr DIV Expr (Prim2("/", Expr1, Expr2))
| Expr EQ Expr  (Prim2("=", Expr1, Expr2))
| Expr INEQ Expr (Prim2("!=", Expr1, Expr2))
| Expr LESS Expr (Prim2("<", Expr1, Expr2))
| Expr LESSEQ Expr (Prim2("<=", Expr1, Expr2))
| Expr DOUBCOLON Expr (Prim2("::", Expr1, Expr2))
| Expr SEMICOL Expr (Prim2(";", Expr1, Expr2))
| Expr LBRACKET Nat RBRACKET (Item(Nat, Expr)) 

AtomicExpr : Const (Const)
| NAME () 
| LBRACE Prog RBRACE (Prog)
| LPARENT Expr RPARENT (Expr)
| LPARENT Comps RPARENT (Comps)
| FN Args DOUBARROW Expr END (Anon(Args, Expr))

AppExpr : AtomicExpr AtomicExpr ()
| AppExpr AtomicExpr ()

Const : TRUE (ConB(TRUE))
| FALSE (ConB(FALSE))
| Nat (Nat)
| LPARENT RPARENT (List([]))
| LPARENT Type LBRACKET RBRACKET RPARENT (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
| Expr COMMA Comps (Expr::Comps)

MatchExpr : END ([])
| PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME  Expr)
| UNDERSCORE (NONE)

Args : LPARENT RPARENT (ListT([]))
| LPARENT Params RPARENT (Params)

Params : TypedVar (#1TypedVar)
| TypedVar COMMA Params (ListT(#1TypedVar::Params::[]))

TypedVar : Type NAME (Type, NAME)

Type : AtomicType (AtomicType)
| LPARENT Types RPARENT (ListT(Types))
| LBRACKET Type RBRACKET (SeqT(Type))
| Type ARROW Type (FunT(Type, Type))

AtomicType : NIL (ListT([]))
| BOOL (BoolT)
| INT (IntT)
| LPARENT Type RPARENT (Type)

Types : Type COMMA Type (Type1::Type2::[])
| Type COMMA Types (Type::Types)

Nat : CINT (ConI(CINT))