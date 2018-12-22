#!/bin/bash

mkdir ResultadosJava

for file in ./Casos/*
do
	java -jar ./Triangle.jar "$file" > "$file".txt
done

mv ./Casos/*.txt ResultadosJava/