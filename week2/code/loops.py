#!/usr/bin/env python3

""""exploring loops """
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##

## constants ##


## functions ##
# FOR loops in pyhton
for i in range(5):
    print(i)
#remember index starts at 0

my_list = [0, 2, "geronimo!", 3.0, True, False]
for k in my_list:
    print(k)
#prints list

total = 0 
summands = [0, 1, 11, 111, 1111]
for s in summands:
    total = total + s
    print(total)
# takes total and sequencially add from summands

# WHHILE loops in python
z = 0
while z < 100:
    z = z + 1
    print(z)
#starting from 0 add 1 untill hit 100

b = True
while b:
    print("geronimo! inifinite loop! crl+c to stop!")
#creates infinate loop