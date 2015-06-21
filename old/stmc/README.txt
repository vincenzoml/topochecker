
BUGS:
=====

The parser only accepts one newline after each semicolon

TODO:
=====

Optimize the model checker avoiding reloading of files

Use OCamlGraph (and parse dot files using their parser)

Establish a file format describing examples and formulas

Split commands from formulas (See Interface.ml, where the ADT for commands is defined, as it is part of the interface)

All comments in English

Remove Test.ml and incorporate its functionality into main.ml


SOURCE FILES
============

parser.mly: Parser for the interpreter and for logic formulas
lexer.mll: For "parser.mly"

parserGraph.mly: parser for DOT files
lexerGraph.mll: For "parserGraph.mly"

Graph.ml: Graphs as quasi-discrete closure spaces
Model.ml: Models of the logic
StlConvert.ml: Conversion of module types
StlLogic.ml: Implementation of the logic, abstract syntax, and model checker

Interface.ml: functions and data types related to the interface/interpreter
Test.ml: loads an example (to be removed!)
main.ml: interpreter
