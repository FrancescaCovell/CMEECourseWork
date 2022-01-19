#!/usr/bin/env python3

""""populate a dictionary derived from  taxa so that it maps order names to sets of taxa"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##

## constants ##


## functions ##
# Write a short python script to populate a dictionary called taxa_dic 
# derived from  taxa so that it maps order names to sets of taxa.
# 
# An example output is:
#  
# 'Chiroptera' : set(['Myotis lucifugus']) ... etc.
#  OR,
# 'Chiroptera': {'Myotis lucifugus'} ... etc

taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

# group taxa based on order
#test[key/order] = set/taxa

#test['Rodentia'] = set([i[0] for i in taxa] if [i][1] == 'Rodentia')
#can get it to work for 1 order

outdict = {y[1]: set([x[0] for x in taxa if x[1] == y[1]]) for y in taxa}
# {} to make it a dictionary comprehemtion
# y[1] ... for y  in taxa loops through taxa and assigns y to be index 1
# set(...) loops through taxa assigns x index 0 if index 1 is equal to y index 1



