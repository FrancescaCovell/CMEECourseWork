#make into program
#uses .csv from directory 

""" Program to compare scores of different sequence alignments """

__author__ = 'Francesca Covell'
__version__ = '0.0.1'

## imports ##

## constants ## ?

## function ##

with open('../data/sequence.csv','r') as f:
    
    csvread =  csv.reader(f)
    test = []
    for row in csvread:
        test.append(row[0])
        print(test)

if l1 >= l2:
    s1 = seq1
    s2 = seq2
else:
    s1 = seq2
    s2 = seq1
    l1, l2 = l2, l1