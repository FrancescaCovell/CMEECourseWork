#!/usr/bin/env python3

"""look at conrtol flow, create 5 function to be called by main function"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'

import sys

def even_or_odd(x=0):

    """Find if number is even or odd"""
    if x % 2 == 0:
        return "%d is even!" % x 
    return "%d is odd!" % x 

def largest_dicisor_five(x=120):
    """find which is the largest divisor range 2-5"""
    largest = 0
    if x % 5 == 0:
        largest = 5
    elif x % 4 == 0:
        largest = 4
    elif x % 3 == 0:
        largest = 3
    elif x % 2 == 0:
        largest = 2
    else: 
        return " No divisor found for %d" % x
    return"the largest divisor of %d id %d" % (x, largest)

def is_prime(x=70):
    """finds prime"""
    for i in range(2,x):
        if x % i == 0:
            print("%d is not prime: %d is a divisor" % (x, i))
            return False
    print("%d is prime" % x)
    return True

def find_all_prime(x=22):
    """finds all primes up to x"""
    allprime = []
    for i in range(2, x + 1):
        if is_prime(i):
            allprime.append(i)
    print("There abd %d primes between 2 and %d" % (len(allprime), x))#type error
    return allprime

def main(argv):
    """main function tests even_or_odd, largest_dicisor_five, is_prime, find_all_prime"""
    print(even_or_odd(22))
    print(even_or_odd(33))
    print(largest_dicisor_five(120))
    print(largest_dicisor_five(121))
    print(is_prime(60))
    print(is_prime(59))
    print(find_all_prime(100))
    return 0

if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)  