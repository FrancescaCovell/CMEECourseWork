#!/bin/bash
#need to remove suffix 

pdflatex .$1tex
bibtex  $1
pdflatex $1.tex
pdflatex $1.tex
evince $1.pdf &

## Cleanup

rm *.aux
rm *.log
rm *bbl
rm *blg
