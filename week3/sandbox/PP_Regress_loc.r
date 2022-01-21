# but the analysis this time should be separate by the dataset’s Location field
# No need to generate plots for this (just the analysis results to a .csv file)
rm(list=ls()) 

require(ggplot2)
require(ggthemes)
require(plyr)

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# Linear Model
# Standadise Data
MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] <- MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] / 1000


linmod <- function(MyDF){
  summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass ))
}


models <- dlply(MyDF, as.quoted(.(Type.of.feeding.interaction, Predator.lifestage, Location)), linmod) 


test2<-ldply(models, function(x) {
  Intercept <- x$coefficients[1]
  Slope <- x$coefficients[2]
  RSquare<- x$r.squared
  pvalue<- x$coefficient[8]
  data.frame(Intercept,Slope,RSquare,pvalue)
}
)  

test3<-ldply(models, function(x){
  Fstatistic <- x$fstatistic[1]
  data.frame(Fstatistic)
}
)

PP_Regress_loc_Results <- merge(test2,test3, by= c("Type.of.feeding.interaction", "Predator.lifestage", "Location"), all = T)
write.csv( PP_Regress_loc_Results, "../results/PP_Regress_loc_Results.csv")
