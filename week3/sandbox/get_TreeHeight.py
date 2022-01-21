#!/usr/bin/env python3


""" Program to compare tree hieghts of input file """

__author__ = 'Francesca Covell'
__version__ = '0.0.1'

import csv
import sys
import math
import pandas as pd

# argument check
if  len(sys.argv) == 1:
    print("At least one input file must be supplies")


args = sys.argv[1]

#Function to calculate Height based on angle and distance 


def TreeHeight (degrees, distance):
"""This function calculates heights of trees given distance of each tree 
from its base and angle to its top, using  the trigonometric formula 
height = distance * tan(radians)

ARGUMENTS
degrees:   The angle of elevation of tree
distance:  The distance from base of tree (e.g., meters)

OUTPUT
The heights of the tree, same units as distance """

  radians = degrees * math.pi / 180
  height = distance * math.tan(radians)
  print("Tree height is:", height)
  
  return height

#impot csv file

data = pd.read_csv(args)
tree = pd.DataFrame(data)

print(tree)

# initialise results table 
Height = [0] * len(tree["Species"])
#run function on csv
for i in  range(len(tree["Species"])):
    Height[i] = TreeHeight(tree["Angle.degrees"][i], tree["Distance.m"][i])

print(Height)
#create output data of results
tree.insert(3, 'Tree.hight.m', Height)

#output dataframe as csv
#remove relative path and suffix 
outputname = {args}
output = {x.replace('.csv', '').replace('../data/', '') for x in outputname}
#create new relative path
outputpath = '../results/'+ ', '.join(output) + '_TreeHts.csv'
tree.to_csv(outputpath)