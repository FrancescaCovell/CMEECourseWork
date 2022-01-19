#!/usr/bin/env python3

""""explores zero division error"""

__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'

## imports ##

## constants ##


## functions ##
def buggyfunc(x):
    """uses try to check for bugs"""
    y = x
    for i in range(x):
        try:        
            y = y-1
            z = x/y
        except ZeroDivisionError:
            print(f"The result of dividing a number by zero is undefined")
        except:
            print(f"This didn't work; x = {x}; y = {y}")
        else:
            print(f"OK; x = {x}; y = {y}, z = {z};")
    return z

buggyfunc(20)