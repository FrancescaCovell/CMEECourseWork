#!/usr/bin/env python3
""" Program to compare scores of different sequence alignments """

__author__ = 'Francesca Covell'
__version__ = '0.0.1'



import csv

with open('../data/407228326.fasta','r') as f:
    content = f.readlines()[1:]
    content = [item.replace("\n","") for item in content]
    seq1 = "".join(content)
f.close()    

with open('../data/407228412.fasta','r') as f:
    content = f.readlines()[1:]
    content = [item.replace("\n","") for item in content]
    seq2 = "".join(content)
f.close()   


l1 = len(seq1)

l2 = len(seq2)
#inputting length of of of the characters into l
if l1 >= l2:
    s1 = seq1
    s2 = seq2
else:
    s1 = seq2
    s2 = seq1
l1, l2 = l2, l1
#if l1 is bigger put seq1 into s1
#else do the opposite

def calculate_score(s1, s2, l1, l2, startpoint):
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

# now try to find the best match (highest score) for the two sequences
my_best_align = []
my_best_score = -1

for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    if z > my_best_score:
        my_best_align.append("." * i + s2) # think about what this is doing!
        my_best_score = z
    elif z == my_best_score:
        my_best_align.append("." * i + s2) # think about what this is doing!
        my_best_score = z 
the_best = str(my_best_score)


textfile = open("my_best_align.txt", "w")

with open ('../results/BestAlignment.txt', 'w') as f:
    for element in my_best_align:
        f.write(element + "\n")
    f.write(s1)
    f.write('\n')
    f.write("Best score:")
    f.write(the_best)