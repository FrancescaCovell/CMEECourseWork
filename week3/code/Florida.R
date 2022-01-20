

rm(list=ls())
require(ggplot2)


# import file
load("../data/KeyWestAnnualMeanTemperature.RData")


png(file="../data/TempByYear.png",
    width= 376, height= 548)
ggplot(ats, aes(x =Year, y= Temp)) + 
  geom_point(size = 2, shape = 1)+
  labs(title="Tempurature recorded by year")
dev.off()

#test correlation
#cor.test(ats$Year,ats$Temp)
test<-cor(ats$Year,ats$Temp)

test1000 <- cor(ats$Year,ats$Temp)

#shuffle Temp colunm 1000, test correlation and store in test1000
Shuffle<-t(lapply(1:1000, function(x) sample(ats$Temp)))
for (x in 1:1000) {
  test1000<- append(test1000, cor(ats$Year,Shuffle[[x]]))
}

#plot and save distibution
png(file="../data/CorrelationFrequency.png",
    width= 376, height= 548)
qplot(test1000,
      geom="histogram",
      binwidth = 0.05,  
      main = "Histogram for Correlation coefficients \n of 1000 random shuffles of Temp", 
      xlab = "Correlation coefficients", 
      ylab = "Fequency",
      fill=I("gold"), 
      col=I("black")) + 
  geom_vline(xintercept = test, color = "red", size=1)
dev.off()

# is observed sig diff to random distibution
Pvalue <- sum(test1000 > test)/sum(test1000)
Pvalue

