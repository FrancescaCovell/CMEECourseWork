#!/usr/bin/env python3
""" Program to compare scores of different sequence alignments """

__author__ = 'Francesca Covell'
__version__ = '0.0.1'


import csv


with open('../data/sequence.csv','r') as f:
    
    csvread =  csv.reader(f)
    seq1 = next(csvread)
    seq2 = next(csvread)
#importing csv for use and inputting line 1 and 2 into seq1 and 2 as list

for x in seq1:
    l1 = len(x)

for x in seq2:
    l2 = len(x)
#inputting length of of of the characters into l

for x in seq1:
    for y in seq2:
        if l1 >= l2:
            s1 = x
            s2 = y
        else:
            s1 = y 
            s2 = x
        l1, l2 = l2, l1
#if l1 is bigger put seq1 into s1
#else do the opposite

def calculate_score(s1, s2, l1, l2, startpoint):
    """ Calculates scores for alignments, inputs two sequences, the lengths of input sequences 
    and start point. Checks in bases of sequences macht and give score based on matches """
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    print("." * startpoint + matched)           
    print("." * startpoint + s2)
    print(s1)
    print(score) 
    print(" ")

    return score

# Test the function with some example starting points:
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

# now try to find the best match (highest score) for the two sequences
my_best_align = None
my_best_score = -1

for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    if z > my_best_score:
        my_best_align = "." * i + s2 # think about what this is doing!
        my_best_score = z 
the_best = str(my_best_score)

with open('../data/BestAlignment.txt', 'w') as f:
    f.write(my_best_align)
    f.write('\n')
    f.write(s1)
    f.write('\n')
    f.write("Best score:")
    f.write(the_best)