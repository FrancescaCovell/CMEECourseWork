

rm(list=ls())

setwd("/home/frcovell/CMEECourseWork/week3/code/")
load("../data/KeyWestAnnualMeanTemperature.RData")
ls()

library(ggplot2)
class(ats)
head(ats)
plot(ats)

png(file="/home/frcovell/CMEECourseWork/week3/data/TempByYear.png",
    width= 376, height= 548)
ggplot(ats, aes(x =Year, y= Temp)) + 
  geom_point(size = 2, shape = 1)+
  labs(title="Tempurature recorded by year")
dev.off()

cor.test(ats$Year,ats$Temp)
test<-cor(ats$Year,ats$Temp)

test1000 <- cor(ats$Year,ats$Temp)


Shuffle<-t(lapply(1:1000, function(x) sample(ats$Temp)))
for (x in 1:1000) {
  test1000<- append(test1000, cor(ats$Year,Shuffle[[x]]))
}


png(file="/home/frcovell/CMEECourseWork/week3/data/CorrelationFrequency.png",
    width= 376, height= 548)
qplot(test1000,
      geom="histogram",
      binwidth = 0.05,  
      main = "Histogram for Correlation coefficients \n of 1000 random shuffles of Temp", 
      xlab = "Correlation coefficients", 
      ylab = "Fequency",
      fill=I("blue"), 
      col=I("red")) + 
  geom_vline(xintercept = test, color = "red", size=1)
dev.off()

# is observed sig diff to random distibution
Pvalue <- sum(test1000>test)
Pvalue

