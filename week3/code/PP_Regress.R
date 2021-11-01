rm(list=ls())
getwd()
setwd("/home/frcovell/CMEECourseWork/week3/code")
require(ggplot2)

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

lm_1 <- summary(lm(MyDF$Type.of.feeding.interaction ~ MyDF$Predator.lifestage, data = MyDF))

## calculate the regression results corresponding to the lines fitted in the
# figure and save it to a csv delimited table called (PP_Regress_Results.csv)
#Linear regression on subsets of the data corresponding to 
# available Feeding Type Predator life Stage combination 

# analysis must be subsetted by the Predator.lifestage field of the dataset
# draws and saves a pdf file of the following figure
# writes the accompanying regression results to a formatted table in csv

##lm predMass vs PreyMass sorted by feeding type

##lm PredMass vs PreyMass sorted by Pred lifestage