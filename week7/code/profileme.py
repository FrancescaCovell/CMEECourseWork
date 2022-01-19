#!/usr/bin/env python3
"""functions to profile code and look at tottime and cumtime"""
__author__ = 'Francesca Covell (francesca.covell21@imperial.ac.uk'
__version__ = '0.0.1'
#__license__ = "License for this code/program"
## imports ##


def my_squares(iters):
    """ take numbers squares and adds to list iterativly"""
    out = []
    for i in range(iters):
        out.append(i ** 2)
    return out


def my_join(iters, string):
    """joins a string ,"""
    out = ''
    for i in range(iters):
        out += string.join(", ")
    return out


def run_my_funcs(x,y):
    """runs above functions"""
    print(x,y)
    my_squares(x)
    my_join(x,y)
    return 0

run_my_funcs(10000000,"My string")