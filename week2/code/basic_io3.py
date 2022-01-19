#!/usr/bin/env python3

"""making a dictionary"""

#__appname__ = '[application name here]'
__author__ = 'Francesca Covell(francesca.covell@imperial.ac.uk)'
__version__ = '0.0.1'
#__license__ = "License for this code/program"

## imports ##

import pickle
## constants ##


## functions ##

#to save an object (even complex) for later use

my_dictionary = {"a key": 10, "another key": 11}


f = open('../data/testp.p','wb')
pickle.dump(my_dictionary, f)
f.close()

# load data again
f = open('../data/testp.p','rb')
another_dictionary = pickle.load(f)
f.close()

print(another_dictionary) # having issue not printing 



