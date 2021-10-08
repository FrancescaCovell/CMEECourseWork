#!/bin/bash
#Author: francescacovell.login@imperial.ac.uk
#Script: UnixPrac1.txt
#Description: improving code
#
#
#Date:6 Oct 2021

#Bash Command Challenge
find . -type f -exec ls -s {} \; | sort -n | head -10
# find = find 
# . = in this directory 
# -type f = things with the type file 
# -exec ls -s = execute a list and identify size
# {} = placeholder for find to insert name
# \ = lets find see the ;
# ; = tells find where to end
# | = redirects, so the output of find acts as input for sort and head
# sort-n = sorts numericly (in this case by file size)
# head -10 = print the first 10 line (can change number to give different outputs)


#1
#Count how many lines there are in each file
NumLines1=`wc -l < $1`#creating variables that count the number of line in file
NumLines2=`wc -l < $2` 
NumLines3=`wc -l < $3`

echo " The file $1 had $NumLines1" #prints the file name and number of lines
echo " The file $2 had $NumLines2"
echo " The file $3 had $NumLines3"

echo # works improvment make it so you can add infinate number of files

#2
#Print everything starting from the second line for the E. coli genome

#3
#Print everything starting from the second line for the E. coli genome

#4
#Count the matches of a particular sequence, “ATGC” in the genome of E. coli

#5
#Compute the AT/GC ratio