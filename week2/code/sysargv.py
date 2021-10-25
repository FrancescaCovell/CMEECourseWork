#!/usr/bin/env python3

""""prints arguments passed to it"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##
import sys
## constants ##


## functions ##

print("This is the name of the script: ", sys.argv[0])
print("Number of arguments: ", len(sys.argv))
print("The arguments are: ", str(sys.argv))
