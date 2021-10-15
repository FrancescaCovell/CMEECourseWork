#!/bin/bash
#Author: francescacovell.login@imperial.ac.uk
#Script: ConcatenateTwoFiles.sh
#description: combines two files into a tird
#
#date: Oct 2021

# improve: make script robust so that it gives feedback to the user 
# and exits if the right inputs are not provided
#cat $1 > $3
#cat $2 >> $3
#echo "Merged File is"
#cat $3

# if 2 file and new filename inputted run
# esle ask for file 
if [[($3 == "")]]
then
    echo "Error! Please input two files to merge and file name to merge into"
else
    cat $1 > ../data/$3
    cat $2 >> ../data/$3
    echo "Merged File is $3"
    echo "$3 saved to data"

fi
# check file types are the same
