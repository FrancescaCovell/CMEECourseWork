#!/bin/bash
#Author: francescacovell.login@imperial.ac.uk
#Script: csctospace.sh 
#Description: substitute the comma in the files with tab
#
#Saves the output into a .txt file
#
#Date:6 Oct 2021


# ask for txt file
# convert file to csv
# if no txt file is give report error
# tell when process is done and where new file is saved
#finished

if [[($1 == *.csv )]]
then
    echo "Converting $1 from csv to txt"
    cat $1 | tr -s "\t" "," >> $(basename "$1" .csv).txt
    echo "done"
    exit
else
    echo "Error! Please input a .csv file"
    exit
fi
