#!/bin/bash

cd ResultadosJava

for file in *
do
	cmp "$file" ../ResultadosOCaml/"$file" >> resultadoComparacion
done

mv resultadoComparacion ../