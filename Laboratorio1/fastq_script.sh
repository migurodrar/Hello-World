#!/bin/bash

# bucle para repetir sobre cada archivo .fastq
for Union in *.fastq; do
   echo "Archivo encontrado: $Union"

#contar el nº de lineas:
   wc -l $Union
done
#mensaje final
echo "Terminado"


