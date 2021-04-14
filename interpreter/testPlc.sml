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
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

use "testPlcCases.sml";

fun testCases (sourceCode:string, expected:string) : string = 
  let
    val expr = fromString(sourceCode);
    val observed = run(expr)
  in
    if (observed = expected) then "V" else sourceCode
  end

val results = map (fn (s,e) => testCases(s, e)) cases;

fun isAllTestsPassed [] = true
  | isAllTestsPassed (h::t) = (h="V") andalso (isAllTestsPassed t);

val isPlcCorrect = isAllTestsPassed(results);
