#!/bin/bash

mkdir temp

cp Misc/*.* temp
cp ContextualAnalyzer/*.* temp
cp SyntacticAnalyzer/*.* temp
cp CodeGenerator/*.* temp

cd temp

ocamlc -c ErrorReporter.mli
ocamlc -c RuntimeEntity.mli
ocamlc -c Ast.mli
ocamlc -c IdentificationTable.mli
ocamlc -c TreeDrawer.mli
ocamlc -c Token.mli
ocamlc -c Parser.mli
ocamlc -c Scanner.mli
ocamlc -c Checker.mli
ocamlc -c Encoder.mli
ocamlc -c TokenPrinter_Pipe.mli
ocamlc -c TokenPrinter_XML.mli
ocamlc -c TokenPrinter_HTML.mli


ocamlc -c ErrorReporter.ml
ocamlc -c IdentificationTable.ml
ocamlc -c TreeDrawer.ml
ocamlc -c Parser.ml
ocamlc -c Scanner.ml
ocamlc -c Checker.ml
ocamlc -c Encoder.ml
ocamlc -c TokenPrinter_Pipe.ml
ocamlc -c TokenPrinter_XML.ml
ocamlc -c TokenPrinter_HTML.ml
ocamlc -c -pp camlp4o Compiler.ml

ocamlc ErrorReporter.cmo IdentificationTable.cmo TreeDrawer.cmo Parser.cmo TokenPrinter_Pipe.cmo TokenPrinter_XML.cmo TokenPrinter_HTML.cmo Scanner.cmo Checker.cmo Encoder.cmo Compiler.cmo -o Triangle

mv Triangle ..
cd ..

rm -r temp