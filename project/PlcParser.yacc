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
    | MatchExpr of (expr option * expr) list
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomicType of plcType
    | Types of plcType list
    | Nat of int
    | Name of string

%right SEMICOL ARROW 
%nonassoc IF
%left ELSE
%left AND
%left EQ INEQ
%left LESS LESSEQ
%right DOUBCOLON
%left PLUS MINUS
%left MULTI DIV
%nonassoc NOT HD TL ISE PRINT
%left RBRACKET

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
| Decl (Decl)

Decl : VAR Name EQ Expr SEMICOL Prog (Let(Name, Expr, Prog))
| FUN Name Args EQ Expr SEMICOL Prog (Let(Name, makeAnon(Args, Expr), Prog))
| FUN REC Name Args COLON Type EQ Expr SEMICOL Prog (makeFun(Name, Args, Type, Expr, Prog)) 

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
| Name (Var(Name)) 
| LBRACE Prog RBRACE (Prog) 
| LPARENT Expr RPARENT (Expr)
| LPARENT Comps RPARENT (List(Comps))
| FN Args DOUBARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomicExpr AtomicExpr (Call(AtomicExpr1, AtomicExpr2)) 
| AppExpr AtomicExpr (Call(AppExpr, AtomicExpr))

Const : TRUE (ConB(TRUE))
| FALSE (ConB(FALSE))
| Nat (ConI(Nat))
| LPARENT RPARENT (List([]))
| LPARENT Type LBRACKET RBRACKET RPARENT (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
| Expr COMMA Comps (Expr::Comps)

MatchExpr : END ([])
| PIPE CondExpr ARROW Expr MatchExpr ((CondExpr,Expr)::MatchExpr)

CondExpr : Expr (SOME(Expr))
| UNDERSCORE (NONE)

Args : LPARENT RPARENT ([])
| LPARENT Params RPARENT (Params)

Params : TypedVar (TypedVar::[])
| TypedVar COMMA Params (TypedVar::Params)

TypedVar : Type Name (Type, Name)

Type : AtomicType (AtomicType)
| LPARENT Types RPARENT (ListT(Types))
| LBRACKET Type RBRACKET (SeqT(Type))
| Type ARROW Type (FunT(Type1, Type2))

AtomicType : NIL (ListT([]))
| BOOL (BoolT)
| INT (IntT)
| LPARENT Type RPARENT (Type)

Types : Type COMMA Type (Type1::Type2::[])
| Type COMMA Types (Type::Types)

Nat: CINT (CINT)

Name: NAME (NAME)