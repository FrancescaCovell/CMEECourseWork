#!/bin/bash

# Run get_TreeHieght R script on trees.csv
Rscript get_TreeHeight.R ../data/trees.csv 

# Run get_TreeHieght Python script on trees.csv
python3 get_TreeHeight.py ../data/trees.csv 