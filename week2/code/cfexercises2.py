#!/usr/bin/env python3

"""examples of combining loops and conditionals"""

#__appname__ = '[application name here]'
__author__ = 'Francesca Covell(francesca.covell@imperial.ac.uk)'
__version__ = '0.0.1'
#__license__ = "License for this code/program"

## imports ##

## constants ##


## functions ##

for j in range(12):
    if j % 3 == 0:
        print('hello')

for j in range(15):
    if j % 5 == 3:
        print('hello')
    elif j % 4 == 3:
        print('hello')

# going through 0-14 if modulo 5 or 4 equals 3 print hello  
z = 0 
while z != 15:
    print('hello')
    z = z + 3
#staring from 0 add 3 and print hello untill z equals 15

z = 12
while z< 100:
    if z == 31:
        for k in range(7):
            print('hello')
    elif z == 18:
        print('hello')
    z = z + 1
#staring from 12 while z is less the 100
#if z equal 31 create a range 7 and print hello
#else if z equals 18 print hello
#add 1 for z 