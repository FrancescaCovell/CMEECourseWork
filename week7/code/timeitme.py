#!/usr/bin/env python3
"""compare speed of list comprehension and loop"""
__author__ = 'Francesca Covell (francesca.covell21@imperial.ac.uk'
__version__ = '0.0.1'
#__license__ = "License for this code/program"
## imports ##
import timeit
import time

iters = 1000000

# loops vs. list comprehensions: which is faster?
from profileme import my_squares as my_squares_loops
from profileme2 import my_squares as my_squares_lc

#loops vs. the join method for strings: which is faster?
mystring = "my string"
from profileme import my_join as my_join_join
from profileme2 import my_join as my_join


# time funtion using time package
start = time.time()
my_squares_loops(iters)
print("my_squares_loops takes %f s to run." % (time.time() - start))
start = time.time()
my_squares_lc(iters)
print("my_squares_lc takes %f s to run." % (time.time() - start))