#!/bin/bash

mkdir ResultadosJavaSinx

for file in ./Casos/*
do
	java -jar ./Triangle.jar "$file"
done

mv ./Casos/*.txt ResultadosJavaSinx/