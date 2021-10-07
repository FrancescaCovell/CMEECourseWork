#!/bin/bash
#Author: francescacovell.login@imperial.ac.uk
#Script: tabtocsv.sh
#description: substitute the tabs in the files with commas
#
#Saves the output into a .csv file
#Arguments: 1 -> tab delimited file
#date: Oct 2021

echo "Creating a comma delimited version of $1 ..."
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
exit