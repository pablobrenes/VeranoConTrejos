#!/bin/bash

mkdir ResultadosOCaml

for file in ./Casos/*
do
	./Triangle "$file" -ptt "$file".txt
done

mv ./Casos/*.txt ResultadosOCaml/