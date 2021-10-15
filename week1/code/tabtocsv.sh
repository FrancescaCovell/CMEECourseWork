#!/bin/bash
#Author: francescacovell.login@imperial.ac.uk
#Script: tabtocsv.sh
#description: substitute the tabs in the files with commas
#
#Saves the output into a .csv file
#Arguments: 1 -> tab delimited file
#date: Oct 2021

#echo "Creating a comma delimited version of $1 ..."
#cat $1 | tr -s "\t" "," >> $1.csv # tack file translate all spaces to commas put in file name 
#echo "Done!"
#exit

# improve: make script robust so that it gives feedback to the user 
# and exits if the right inputs are not provided

if [[($1 == *.txt)]]
then
    echo "Creating a comma delimited version of $1 ..."
    cat $1 | tr -s "\t" "," >> $(basename "$1" .txt).csv # tack file translate all spaces to commas put in file name 
    echo "Done!"
    exit
else 
    echo " Error! Please input .txt file"
    exit
fi

# quiry should this be able to be used for not txt files?
