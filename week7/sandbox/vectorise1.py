#!/usr/bin/env python3
"""Description of this program or application.
You can use several lines"""
__author__ = 'Francesca Covell (francesca.covell21@imperial.ac.uk'
__version__ = '0.0.1'
#__license__ = "License for this code/program"
## imports ##
import numpy as np
import timeit
import matplotlib.pylab as p

## Loop vs LC

def loop_product(a, b):
    """"mulitplying a by b and put into c"""
    N = len(a) #pralocating
    c = np.zeros(N)
    for i in range(N):
        c[i] = a[i] * b[i]
    return c

def vect_product(a,b):
    """mulitplying a by b and put into c"""
    return np.multiply(a,b)


## Runtimes

array_lengths = [1, 100, 10000, 1000000, 10000000]
t_loop = []
t_vect = []
for N in array_lengths:
    print("\nSet N=%d" %N)
#randomly generate our 1D arrays of length N
    a = np.random.rand(N)
    b = np.random.rand(N)
# time loop_product 3 times and save the mean execution time.
    timer = timeit.repeat('loop_product(a, b)', globals=globals().copy(), number=3)
    t_loop.append(1000 * np.mean(timer))
    print("Loop method took %d ms on average." %t_loop[-1])
# time vect_product 3 times and save the mean execution time.
    timer = timeit.repeat('vect_product(a, b)', globals=globals().copy(), number=3)
    t_vect.append(1000 * np.mean(timer))
    print("vectorized method took %d ms on average." %t_vect[-1])

## plotting Loop vd LC

p.figure()
p.plot(array_lengths, t_loop, label="loop method")
p.plot(array_lengths, t_vect, label="vect method")
p.xlabel("Array length")
p.ylabel("Execution time (ms)")
p.legend()
p.show()


## test memory 
N = 1000000000
a = np.random.rand(N)
b = np.random.rand(N)
c = vect_product(a, b)
# if no error, remove a, b, c from memory.
del a
del b
del c