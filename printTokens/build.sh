#!/bin/bash

mkdir temp

cp codigo/*.ml temp
cp codigo/*.mli temp

cd temp

ocamlc -c ErrorReporter.mli
ocamlc -c TokenDeclaration.mli
ocamlc -c TokenPrinter.mli
ocamlc -c Scanner.mli

ocamlc -c ErrorReporter.ml
ocamlc -c TokenPrinter.ml
ocamlc -c Scanner.ml
ocamlc -c main.ml

ocamlc ErrorReporter.cmo TokenPrinter.cmo Scanner.cmo main.cmo -o printToken

mv printToken ..
cd ..

rm -r temp