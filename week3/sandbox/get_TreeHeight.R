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

# Take inputs from command line 
args = commandArgs(trailingOnly = TRUE) # Provides access to a copy of the command line arguments 
                                        # trailingOnly = TRUE: character vector of arguments supplied after --args

# argument check
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE) #stop current expression. call.=FALSE stops error message containing a reference to hidden function
} else if (!grepl("\\.csv$", args)) { #check file suffix
  stop("Agunent must be .csv", call.=FALSE) 
}

#Function to calculate Height based on angle and distance 
TreeHeight <- function(degrees, distance){
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  print(paste("Tree height is:", height))
  
  return(height)
}



#import csv file
tree <- read.csv(args)

#run function on csv 
Height <-vector() 
for (i in 1:length(tree$Species)) {
  Height[i]<-TreeHeight(tree$Angle.degrees[i], tree$Distance.m[i])
  
}

#create data frame of results 
TreeHts<- data.frame(Species =  tree$Species,
                     Distance.m = tree$Distance.m,
                     Angle.degrees = tree$Angle.degrees,
                     Tree.hight.m = Height )

#output dataframe as csv
#remove relative path and suffix 
outputname <- gsub("\\.csv$","", args, ignore.case= TRUE)
outputname <- gsub("\\../data/","", outputname, ignore.case = TRUE)
#create new relative path
output <- paste0("../results/", outputname,"_TreeHts.csv")
write.csv( TreeHts,  output)

