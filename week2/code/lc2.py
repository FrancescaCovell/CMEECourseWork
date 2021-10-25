#!/usr/bin/env python3

""""writing list comprehension and loops for rainfall"""
#dacstrins are considered apart of the running code (normal comments are
#stripped), Hence you can access your docstrings iat run time.
__author__ = 'Francesca Covell (francesca.covell@imperial.ac.uk)' #change to full email
__version__ = '0.0.1'
## imports ##

## constants ##


## functions ##
# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.
greater_than_100_lc = set([i for i in rainfall if i[1] > 100])
print("Months and rainfall values when the amount of rain was greater than 100mm", greater_than_100_lc)

# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm. 
less_than_50_lc = set([i for i in rainfall if i[1]<50])
print("Month and rainfall values when the amount of rain was less then 50mm", less_than_50_lc)


# (3) Now do (1) and (2) using conventional loops (you can choose to do 
# this before 1 and 2 !). 
greater_than_100 = []

for i in rainfall:
    if i[1] > 100:
     greater_than_100.append (i)
print("Months and rainfall values when the amount of rain was greater than 100mm", greater_than_100)

less_than_50 = []
for i in rainfall:
    if i[1] < 50:
     less_than_50.append (i)
print("Months and rainfall values when the amount of rain was less than 50mm", less_than_50)


