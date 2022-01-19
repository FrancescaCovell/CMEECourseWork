#!/usr/bin/env python3

""""example script to be run"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##

## constants ##


## functions ##
def foo(x):
    """" times input by itself and prints answer"""
    x *= x # same as x = x*x
    print(x)

foo(2)