# Birds is a tuple of tuples of length three: latin name, common name, mass.
# write a (short) script to print these on a separate line or output block by species 
# 
# A nice example output is:
# 
# Latin name: Passerculus sandwichensis
# Common name: Savannah sparrow
# Mass: 18.7
# ... etc.

# Hints: use the "print" command! You can use list comprehensions!

birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
        )
        
#### tests ####

#    print("Latin name:",[i[0] for i in birds], "Common name:",
#    [i[1] for i in birds], "Mass:", [i[2] for i in birds])
#prints all latin names then all common then all mass


#for x in birds:
 #   print("latin name:", birds [x[0]),
  #  print("Common:", birds [x[1]),
  #  print("mass", birds [x[2]),

# be careful not to use , at the end of lines, indicates line is continuing
# dont need to put birds into loop, asking to index birds from characters

#### solution ####

for x in birds:
    print("latin name:", x[0])
    print("Common:", x[1])
    print("mass", x[2])