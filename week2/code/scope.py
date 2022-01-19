#!/usr/bin/env python3

"""Redefining a_functionto explore how global and local variables,
 interact with fuction"""

#__appname__ = '[application name here]'
__author__ = 'Francesca Covell(francesca.covell@imperial.ac.uk)'
__version__ = '0.0.1'
#__license__ = "License for this code/program"

## imports ##

## constants ##


## functions ##
#a_function



# variable scope

# first example 

_a_global = 10 # a global variable

if _a_global >= 5:
    _b_global = _a_global + 5 # also a global variable

def a_function():
    """Check valie of variable inside function"""
    _a_global = 5 # a local variable

    if _a_global >= 5:
        _b_global = _a_global + 5 # also a local variable
    _a_local = 4

    print("Inside the function, the value of _a_global is ", _a_global)
    print("Inside the function, the value of _b_global is ", _b_global)
    print("Inside the function, the value of _a_local is ", _a_local)

    return None

a_function()

print("Outside the function, the value of _a_global is ", _a_global)
print("Outside the function, the value of _b_global is ", _b_global)

#example 2

_a_global = 10

def a_function():
    """shows function can see global and local variables inside function"""
    _a_local = 4

    print("Inside the function, the value _a_local is ", _a_local) 
    print("Inside the function, the value of _a_global is ", _a_global)

    return None 

a_function()

print("Outside the function, the value of _a_global is", _a_global)



# example 3

_a_global = 10

print("Outside the function, the value of _a_global is", _a_global)

def a_function():
    """show global variable can be change ithing a function"""
    global _a_global
    _a_global = 5
    _a_local = 4
    
    print("Inside the function, the value of _a_global is ", _a_global)
    print("Inside the function, the value _a_local is ", _a_local)
    
    return None

a_function()

print("Outside the function, the value of _a_global now is", _a_global)

# example 4

def a_function():
    """initialises global variable, calls _a_function2 and returns none
    global variable doesnt change untill return statment"""
    _a_global = 10

    def _a_function2():
        """change global variable"""
        global _a_global
        _a_global = 20

    print("Before calling a_function, value of _a_global is ", _a_global)

    _a_function2()

    print("After calling _a_function2, value of _a_global is ", _a_global)

    return None

a_function()

print("The value of a_global in main workspace / namespace is ", _a_global)

#example 5 

_a_global = 10

def a_function():
    """initialises global variable, calls _a_function2 with no return statment
    global variable changes in _a_function2"""
    def _a_function2():
        """change global variable"""
        global _a_global
        _a_global = 20

    print("Before calling a_function, value of _a_global is ", _a_global)

    _a_function2()

    print("After calling _a_function2, value of _a_global is ", _a_global)

a_function()

print("The value of a_global in main workspace / namespace is ", _a_global)