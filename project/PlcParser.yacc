%%

%name PlcParser

%pos int

%term SEMICOL | COLON | DOUBCOLON | COMMA
    | VAR
    | EQ | INEQ | LESS | LESSEQ
    | FUN | REC
    | IF | THEN | ELSE
    | MATCH | WITH
    | EXCLAMATION
    | MINUS | PLUS | MULTI | DIV
    | HD | TL
    | ISE
    | PRINT
    | BINOP
    | LBRACE | RBRACE
    | LBRACKET | RBRACKET
    | LPARENT | RPARENT
    | DOUBARROW
    | END
    | TRUE | FALSE
    | PIPE
    | NIL | BOOL | INT
    | NOT

%nonterm Prog of expr
    | Decl of 
    | Expr of expr
    | AtomicExpr of 
    | AppExpr of 
    | Const of 
    | Comps of 
    | MatchExpr of 
    | CondExpr of 
    | Args of 
    | Params of 
    | TypedVar of 
    | Type of 
    | AtomicType of 
    | Types of 
    | Name of 
    | Nat of 

%right SEMICOL ARROW DOUBCOLON
%left ELSE BINOP EQ INEQ LESS LESSEQ PLUS MINUS MULTI DIV RBRACKET
%nonassoc IF NOT HD TL ISE PRINT

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr 
| Decl SEMICOL Prog 

Decl : VAR Name EQ Expr 
| FUN Name Args EQ Expr 
| FUN REC Name Args COLON Type EQ Expr

Expr : AtomicExpr
| AppExpr
| IF Expr THEN Expr ELSE Expr
| MATCH Expr WITH MatchExpr
| EXCLAMATION Expr
| MINUS Expr
| HEAD Expr
| TAIL Expr
| ISE Expr
| PRINT Expr
| Expr BINOP Expr
| Expr PLUS Expr
| Expr MINUS Expr
| Expr MULTI Expr
| Expr DIV Expr
| Expr EQ Expr
| Expr INEQ Expr
| Expr LESS Expr
| Expr LESSEQ Expr
| Expr DOUBCOLON Expr
| Expr SEMICOL Expr
| Expr LBRACKET Nat RBRACKET 

AtomExpr : Const
| Name
| LBRACE Prog RBRACE
| LPARENT Expr RPARENT
| LPARENT Comps RPARENT
| FN Args DOUBARROW Expr END

AppExpr : AtomExpr AtomExpr
| AppExpr AtomExpr

Const : TRUE
| FALSE
| Nat
| LPARENT RPARENT
| LPARENT Type LBRACKET RBRACKET RPARENT

Comps : Expr COMMA Expr
| Expr COMMA Comps

MatchExpr : END
| PIPE CondExpr ARROW Expr MatchExpr

CondExpr : Expr
| MINUS

Args : LPARENT RPARENT
| LPARENT Params RPARENT

Params : TypedVar
| TypedVar COMMA Params

TypedVar : Type Name

Type : AtomicType
| LPARENT Types RPARENT
| LBRACKET Type RBRACKET
| Type ARROW Type

AtomicType : NIL
| BOOL
| INT
| LPARENT Type RPARENT

Types : Type COMMA Type
| Type COMMA Types