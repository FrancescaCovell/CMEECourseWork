# Finds just those taxa that are oak trees
taxa = ['Quercus robur',
        'Fraxinus excelsior',
        'Pinus sylvestris',
        'Quercus cerris',
        'Quercus petraea',
        ]

def is_an_oak(name):
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