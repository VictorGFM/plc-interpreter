%%

%name PlcParser

%pos int

%term VAR
    | FUN | REC
    | IF | THEN | ELSE
    | MATCH | WITH
    | DIF | HIF 
    | HEAD | TAIL
    | ISE
    | PRINT
    | AND
    | PLUS | MINUS | MULTI | DIV
    | EQ | DIF 
    | SMALLER | SMALL_OR_EQ
    | CONSTRUCT_LIST
    | SEMICOL
    | CINT of Int | NAME of string
    | LBRACKET | RBRACKET
    | LPARENT | RPARENT
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

%eop EOF

%noshift EOF

%start Prog

%%
