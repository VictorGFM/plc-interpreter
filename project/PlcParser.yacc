%%

%name PlcParser

%pos int

%term VAR
    | FUN | REC
    | IF | THEN | ELSE
    | MATCH | WITH
    | REF | HIF 
    | HEAD | TAIL
    | ISE
    | PRINT
    | AND
    | PLUS | MINUS | MULTI | DIV
    | EQ | DIF 
    | SMALLER | SMALL_OR_EQ
    | CONSTRUCT_LIST
    | SEMICOL | COL
    | CINT of int | NAME of string
    | LBRACKET | RBRACKET
    | LPARENT | RPARENT
    | TRUE of bool | FALSE of bool
    | EOF
    //Fiz o "mapeamento" até a parte  <atomic expr>::= <const>

%nonterm Prog of expr
            | Expr of expr
            | Decl of expr
            | Args of plcVal
            | Name of plcType
            | AtomExpr of expr
            | AppExpr of expr
            | Const of plcVal
            | Comps of expr
            | MatchExpr of expr
            | CondExpr of expr
            | Params of expr
            | TypedVar of plcType
            | Type of plcType
            | AtomType of plcType
            | Types of plcType

%right SEMICOL
%left EQ PLUS MINUS MULTI DIV
%nonassoc

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
| Decl SEMICOL Prog (Let(NAME,Expr, Prog))

Decl : VAR NAME EQ Expr ()
| FUN NAME Args EQ Expr () 
| FUN REC NAME COL Type EQ Expr()

Args : LPARENT RPARENT ()
| LPARENT Params RPARENT (Params)

Const : TRUE 
| FALSE 
| CINT (ConI)
| LPARENT RPARENT
| LPARENT LBRACKET Type RBRACKET RPARENT