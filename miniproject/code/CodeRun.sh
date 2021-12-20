#!/bin/bash

#Author: francescacovell.login@imperial.ac.uk
#Script: ScriptRun.sh
#description: Runs Python and R Script
#
#date: Dec 2021

# run data wrangling 
python3 PopGrowthData.py 
# run model
Rscript PopGrowthModle.R 
# run results
Rscript PopGrowthResult.R

# count words in report
texcount -1 -sum=1,2 report.tex > words.sum

rm *.pdf
# LaTeX report
echo "Compiling LaTeX"
pdflatex report.tex
bibtex report.aux
pdflatex report.tex
pdflatex report.tex
echo "Compiled LaTeX report"



