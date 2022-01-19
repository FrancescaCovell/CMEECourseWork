#!/usr/bin/env python3
#!/usr/bin/env python3
"""functions to look at prallocation"""
__author__ = 'Francesca Covell (francesca.covell21@imperial.ac.uk'
__version__ = '0.0.1'
#__license__ = "License for this code/program"
## imports ##
import numpy as np


# def my_squares(iters):
#     """squares every number in a range"""
#     out = [i ** 2 for i in range(iters)]
#     return out


# def my_join(iters, string):
#     """joining a , and string to out"""
#     out = ''
#     for i in range(iters):
#         out += ", " + string
#     return(out)

# def run_my_funcs(x,y):
#     """runs my_square on x and my_join on x and y"""
#     print(x, y)
#     my_squares(x)
#     my_join(x,y)
#     return 0

# run_my_funcs(10000000,"My string")


## Using preallocation
#pre allocate 10000000
def my_squares(iters):
    """squares every number in a range"""
    out = np.zeros((1,10000000))
    out = [i ** 2 for i in range(iters)]
    return out


def my_join(iters, string):
    """joining a , and string to out"""
    out = ''
    for i in range(iters):
        out += ", " + string
    return(out)

def run_my_funcs(x,y):
    """runs my_square on x and my_join on x and y"""
    print(x, y)
    my_squares(x)
    my_join(x,y)
    return 0

run_my_funcs(10000000,"My string")