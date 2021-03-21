(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fromString "15";
fromString "true";
fromString "()";
fromString "(6,false)[1]";
fromString "([Bool] [])";
fromString "print x; true";
fromString "3::7::t";
fromString "fn (Int x) => -x end";
fromString "var x = 9; x + 3";
fromString "fun f(Int x) = x; f(1)";
fromString "match x with | 0 -> 1| _ -> -1 end";
(*fromFile ("example.plc");*)

use "testParserCases.sml";

fun testCases (sourceCode:string, expected:expr) : string = 
  let
    val observed = fromString sourceCode    
  in
    if (observed = expected) then "V" else sourceCode
  end

val results = map (fn (s,e) => testCases(s, e)) cases;

fun isAllTestsPassed [] = true
  | isAllTestsPassed (h::t) = (h="V") andalso (isAllTestsPassed t);

val isParserCorrect = isAllTestsPassed(results);
