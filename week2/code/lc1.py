#!/usr/bin/env python3

""""writing list comprehension and loops for Birds"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##

## constants ##


## functions ##
birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively. 

latin_name_lc = set([i[0] for i in birds])

common_name_lc = set([i[1] for i in birds])

mean_body_mass_lc = set([i[2] for i in birds])
# set is an unordered collection of items
# every element unique and immutable
# set itself is mutable

# (2) Now do the same using conventional loops (you can choose to do this 
# before 1 !).


for i in birds:
     latin_name_loop = set([i[0]])
print(latin_name_loop)

for i in birds:
     common_name_loop = set([i[1]])
print(common_name_loop)

for i in birds:
     body_mass_loop = set([i[2]])
print(body_mass_loop)



# A nice example out out is:
# Step #1:
# Latin names:
# ['Passerculus sandwichensis', 'Delichon urbica', 'Junco phaeonotus', 'Junco hyemalis', 'Tachycineata bicolor']
# ... etc.
 