#!/bin/bash

cd ResultadosJavaSinx

for file in *
do
	cmp "$file" ../ResultadosOCamlSinx/"$file" >> resultadoComparacion
done

mv resultadoComparacion ../
