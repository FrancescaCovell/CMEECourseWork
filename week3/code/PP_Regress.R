rm(list=ls())
getwd()


require(ggplot2)
require(ggthemes)
require(plyr)

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

#Graph

pdf("../results/pp_Regress.pdf")
ggplot(MyDF, aes(x = (Prey.mass), y = (Predator.mass),
                     colour = Predator.lifestage )) +
                     geom_point(size=I(2), shape=I(10)) + 
                     theme_bw() +  
                     geom_smooth(method = "lm", fullrange = TRUE) +
                     scale_x_log10()+
                     scale_y_log10()+ 
                     theme(aspect.ratio = 0.5,
                           legend.position = "bottom",
                           legend.title = element_text(size=8,face="bold"),
                           legend.text=element_text(size=8),
                           strip.text=element_text(size=6))+
                     labs(x = "Prey Mass in grams", y = "Predator mass in grams") +
                     guides(colour = guide_legend(nrow=1))                   
                     facet_wrap( .~ Type.of.feeding.interaction, nrow = 5)
graphics.off()



# Linear Model
# Standadise Data
MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] <- MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] / 1000


linmod <- function(MyDF){
  summary(lm(MyDF$Predator.mass ~ MyDF$Prey.mass ))
}
models <- dlply(MyDF, as.quoted(.(Type.of.feeding.interaction, Predator.lifestage)), linmod)


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

PP_Regress_Results <- merge(test2,test3, by= c("Type.of.feeding.interaction", "Predator.lifestage"), all = T)
write.csv( PP_Regress_Results, "../results/PP_Regress_Results.csv")
