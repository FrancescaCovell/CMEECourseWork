#!/bin/bash
#need to remove suffix 

pdflatex $(basename -s .tex $1).tex
bibtex  $(basename -s .tex $1)
pdflatex $(basename -s .tex $1).tex
pdflatex $(basename -s .tex $1).tex
evince  $(basename -s .tex $1).pdf &
# $() allows basename -s to remove suffix before pdflatex etc. activates

## Cleanup

rm *.aux
rm *.log
rm *bbl
rm *blg
