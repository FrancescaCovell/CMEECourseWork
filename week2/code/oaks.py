#!/usr/bin/env python3

""""writing list comprehension and loops for taxa"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##

## constants ##


## functions ##
# Finds just those taxa that are oak trees
taxa = ['Quercus robur',
        'Fraxinus excelsior',
        'Pinus sylvestris',
        'Quercus cerris',
        'Quercus petraea',
        ]

def is_an_oak(name):
    """returns all imstances of 'quercus ' in input (name) """
    return name.lower().startswith('quercus ')
#can't get to work

# using for loops
oak_loops = set()
for species in taxa:
    if is_an_oak(species):
        oak_loops.add(species)
print(oak_loops)

# using list comprehensions
oak_lc = set([species for species in taxa if is_an_oak(species)])
print(oak_lc)

# get names in upper case using loop
oak_loops = set()
for species in taxa:
    if is_an_oak(species):
        oak_loops.add(species.upper())
print(oak_loops)

#get name in upper using lc
oak_lc = set([species.upper() for species in taxa if is_an_oak(species)])
print(oak_lc)