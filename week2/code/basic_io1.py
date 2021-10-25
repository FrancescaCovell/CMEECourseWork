#!/usr/bin/env python3

"""differnt itterations of opening a txt file"""

#__appname__ = '[application name here]'
__author__ = 'Francesca Covell(francesca.covell@imperial.ac.uk)'
__version__ = '0.0.1'
#__license__ = "License for this code/program"

## imports ##

## constants ##


## functions ##



#############################
# FILE INPUT
#############################
# Open a file for reading
f = open('../data/test.txt', 'r')
# use "implicit" for loop:
# if the object is a file, python will cycle over lines
for line in f:
    print(line)

# close the file
f.close()

#Same example, skip blank lines
f = open('../data/test.txt', 'r')
for line in f:
    if len(line.strip()) > 0:
        print(line)

f.close()

#open using with


with open('../data/test.txt', 'r') as f:
# use "implicit" for loop:
# if the object is a file, python will cycle over lines
  for line in f:
    print(line)


with open('../data/test.txt','r') as f:
  for line in f:
      if len(line.strip()) > 0:
        print(line)
        