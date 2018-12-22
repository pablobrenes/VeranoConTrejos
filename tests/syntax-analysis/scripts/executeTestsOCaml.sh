#!/bin/bash

mkdir ResultadosOCamlSinx

for file in ./Casos/*
do
	./Triangle "$file" -xt "$file".txt
done

mv ./Casos/*.txt ResultadosOCamlSinx/