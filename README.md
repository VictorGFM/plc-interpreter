# plc-interpreter

Plc programming language interpreter developed in the UFMG Programming Language discipline.

## Setup

To run this project, sml, ml-yacc and ml-lex must be installed.

Run the following commands to generate the lexer and parser modules:

```bash
ml-yacc PlcParser.yacc
ml-lex PlcLexer.lex
```

Then the interpreter can be tested by running the testPlc.sml file with the following command:

```bash
sml testPlc.sml
```
