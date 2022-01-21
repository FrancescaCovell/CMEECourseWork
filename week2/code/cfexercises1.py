#!/usr/bin/env python3

"""exploring different conditionals"""

#__appname__ = '[application name here]'
__author__ = 'Francesca Covell(francesca.covell@imperial.ac.uk)'
__version__ = '0.0.1'
#__license__ = "License for this code/program"

## imports ##

## constants ##


## functions ##
# What does each of foo_x do?

def foo_1(x):
    """foo_1 times x by the power 0.5"""
    return x ** 0.5


def foo_2(x, y):
    """ foo_2 returns the largest number out of inpust (x, y)"""
    if x > y:
        return x 
    return y


def foo_3(x, y, z):
    """ foo_3 re orders lowest to highest out of input (x, y, z)"""
    if x > y:
        temp = y
        y = x 
        x = temp
    if y > z:
        temp = z
        z = y
        y = temp
    return [x, y, z]


def foo_4(x):
    """ foo_4 creates a list range 0 - x, + 1 and times them by eachother"""
    result = 1
    for i in range(1, x + 1):
        result = result * i
    return result


def foo_5(x):
    """ foo_5 returns 1 if 1 inputted, else returns x times x - 1"""
    if x == 1:
        return 1
    return x * foo5(x - 1) # double check original foo5(error not defined)


def foo_6(x): 
    """foo_6 give factorial, while x is greater the 1 times x by 1 and add to facto
    then reduce x by 1 """
    facto = 1
    while x >= 1:
        facto = facto * x 
        x = x -1
    return facto

