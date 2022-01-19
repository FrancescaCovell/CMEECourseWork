#!/usr/bin/env python3

""""understanding imports and __name__"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##

## constants ##


## functions ##
# Filename: using_name.py

if __name__ == '__main__':
    print ('This program is being run by itself')
else:
    print('I am being imported from another module')

print("This module's name is: " +__name__)
