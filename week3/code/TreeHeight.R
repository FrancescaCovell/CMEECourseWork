## Example Function ##

# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"


TreeHeight <- function(degrees, distance){
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  #print(paste("Tree height is:", height))
  
  return(height)
}


trees <- read.csv("../data/trees.csv")
#import data
Tree.Height.m <- TreeHeight(trees$Angle.degrees, trees$Distance.m)
#run function on data and put into new variable
TreeHts <- read.csv("../data/trees.csv") 
#make csv file of data
TreeHts <- cbind(TreeHts, Tree.Height.m)
#add column of results from function
write.csv(TreeHts, "../results/TreeHts.csv")
#export to results
