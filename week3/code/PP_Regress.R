rm(list=ls())
getwd()
setwd("/home/frcovell/CMEECourseWork/week3/code")

require(ggplot2)
require(ggthemes)
require(plyr)

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")


#x <- dlply(MyDF, as.quoted(.(Type.of.feeding.interaction, Predator.lifestage)))
#lm_test <- summary(lm(lm(x$`insectivorous.larva / juvenile`$Predator.mass ~ x$`insectivorous.larva / juvenile`$Prey.mass)))
#lm_test[2][1]
#p <- plot(x$`insectivorous.larva / juvenile`$Predator.mass, x$`insectivorous.larva / juvenile`$Prey.mass)
#abline(lm(x$`insectivorous.larva / juvenile`$Predator.mass ~ x$`insectivorous.larva / juvenile`$Prey.mass))

# Standadise Data
MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] <- MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] / 1000

#for (x in which(MyDF$Prey.mass.unit == 'mg')) {
 # MyDF$Prey.mass[x] <-  MyDF$Prey.mass[x] /1000
  
#}

linmod <- function(MyDF){
  summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass ))
}
models <- dlply(MyDF, as.quoted(.(Type.of.feeding.interaction, Predator.lifestage)), linmod)

#extract coefficients
test<-ldply(models, coef)
test

# scale change
# standardized mass


x



p <- ggplot(MyDF, aes(x = Prey.mass, y = Predator.mass,
                      colour = Predator.lifestage )) +
                     geom_point(size=I(2), shape=I(10)) + 
  theme_bw() +  
  scale_x_log10(labels = (scientific))+
  scale_y_log10(labels = (scientific)) +
  #scale_y_continuous(limits = c(1e-06, 1e+06)) + scale_y_log10() + scale_y_continuous(labels = scientific)+
  #scale_x_continuous(limits = c(1e-07, 1e+01)) + scale_x_log10()+ scale_x_continuous(labels = scientific)+
  facet_wrap( .~ Type.of.feeding.interaction, nrow = 5)


p


  theme_bw()
library(scales)


#give correct scale for x axis on y axis
p <- ggplot(MyDF, aes(x = Predator.mass, y = Prey.mass,
                      colour = Predator.lifestage )) +
  geom_point(size=I(2), shape=I(10)) + 
  theme_bw() +  
  scale_y_continuous(limits = c(1e-06, 1e+06)) + scale_y_log10() + scale_y_continuous(labels = scientific)+
  scale_x_continuous(limits = c(1e-07, 1e+01)) + scale_y_log10()+ scale_x_continuous(labels = scientific)+
  facet_wrap( .~ Type.of.feeding.interaction, nrow = 5)


p




## calculate the regression results corresponding to the lines fitted in the
# figure and save it to a csv delimited table called (PP_Regress_Results.csv)
#Linear regression on subsets of the data corresponding to 
# available Feeding Type Predator life Stage combination 

# analysis must be subsetted by the Predator.lifestage field of the dataset
# draws and saves a pdf file of the following figure
# writes the accompanying regression results to a formatted table in csv


