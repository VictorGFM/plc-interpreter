(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) = 
	case s of 
    "var" => VAR(lpos, rpos)
    | "Bool" => BOOL(lpos, rpos)
    | "else" => ELSE(lpos, rpos)
    | "end" => END(lpos, rpos)
    | "false" => FALSE(false, lpos, rpos)
    | "fn" => FN(lpos, rpos)
    | "fun" => FUN(lpos, rpos)
    | "hd" => HD(lpos, rpos)
    | "if" => IF(lpos, rpos)
    | "Int" => INT(lpos, rpos)
    | "ise" => ISE(lpos, rpos)
    | "match" => MATCH(lpos, rpos)
    | "Nil" => NIL(lpos, rpos)
    | "print" => PRINT(lpos, rpos)
    | "rec" => REC(lpos, rpos)
    | "then" => THEN(lpos, rpos)
    | "tl" => TL(lpos, rpos)
    | "true" => TRUE(true, lpos, rpos)
    | "with" => WITH(lpos, rpos)
    | "_" => (UNDERSCORE(lpos, rpos))
    | _ => NAME(s,lpos, rpos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun strToInt s =
	case Int.fromString s of
		  SOME i => i
	  | NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()

%%

%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
whitespace=[\ \t];
nat=[0-9]+;
name=['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*;

%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{nat} => (CINT(strToInt(yytext), yypos, yypos));
{name} => (keyword(yytext, yypos, yypos));
"+" => (PLUS(yypos,yypos));
"-" => (MINUS(yypos,yypos));
"*" => (MULTI(yypos, yypos));
"/" => (DIV(yypos, yypos));
"{" => (LBRACE(yypos, yypos));
"}" => (RBRACE(yypos, yypos));
"[" => (LBRACKET(yypos, yypos));
"]" => (RBRACKET(yypos, yypos));
"(" => (LPARENT(yypos, yypos));
")" => (RPARENT(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (INEQ(yypos, yypos));
"&&" => (AND(yypos, yypos));
"!" => (NOT(yypos, yypos));
"," => (COMMA(yypos, yypos));
";" => (SEMICOL(yypos, yypos));
"->" => (ARROW(yypos, yypos));
"=>" => (DOUBARROW(yypos, yypos));
"<" => (LESS(yypos, yypos));
"<=" => (LESSEQ(yypos, yypos));
":" => (COLON(yypos, yypos));
"::" => (DOUBCOLON(yypos, yypos));
. => (error("\n***Lexer error: bad character ***\n"); raise Fail("Lexer error: bad character "^yytext));