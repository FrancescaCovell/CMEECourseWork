#!/usr/bin/env python3

""" modify a txt file"""

#__appname__ = '[application name here]'
__author__ = 'Francesca Covell(francesca.covell@imperial.ac.uk)'
__version__ = '0.0.1'
#__license__ = "License for this code/program"

## imports ##

## constants ##


## functions ##


#save the elements to a list of a file
list_to_save = range(100)

f = open('../data/testout.txt','w')
for i in list_to_save:
    f.write(str(i) + '\n')

f.close()

