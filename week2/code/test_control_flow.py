#!/usr/bin/env python3

"""some functions exemplifying the use of control statment"""

__author__ = 'Francesca Covell(francesca.covell@imperial.ac.uk)'
__version__ = '0.0.1'

import sys
import doctest

def even_or_odd(x=0):
    """find whether a number x is even or odd.
    
    >>> even_or_odd(10)
    '10 is Even!'

    >>> even_or_odd(5)
    '5 is Odd!'

    whenever a float is provided, then the closest integer is used:
    >>> even_or_odd(3.2)
    '3 is odd'

    in case of negative numbers, the positive is taken:
    >>> even_or_odd(-2)
    '-2 is Even!'

    """
    #define funcion to be tested
    if x % 2 == 0:
        return "%d is Even!" % x
    return "%d is Odd!" % x

def main(argv):
    """ function to call, run and test even_or_odd"""
    print(even_or_odd(22))
    print(even_or_odd(33))
    return 0

if  (__name__ == "__main__"):
    status = main(sys.argv)

doctest.testmod()