

rm(list=ls())
require(ggplot2)


# import file
load("../data/KeyWestAnnualMeanTemperature.RData")

# creat matrix of temp in successive year and test correlation

H <- matrix(ncol=2, nrow=(length(ats$Year)-1))

for (i in 1:(length(ats$Year)-1)){
  H[i,1] <- ats$Temp[i]
  H[i,2] <- ats$Temp[i+1]
}
  initial_cor<-cor(H[,1], H[,2])
  test_year_1000<-cor(H[,1], H[,2])


#shuffle second column 1000, test correlation and store
Shuffle<-t(lapply(1:1000, function(x) sample(H[,2])))
for (x in 1:1000) {
  test_year_1000<- append(test_year_1000, cor(H[,1],Shuffle[[x]]))
}

#plot and save distibution
png(file="../data/TempBetweenTime.png",
    width= 376, height= 548)
qplot(test_year_1000,
      geom="histogram",
      binwidth = 0.05,  
      main = "Histogram for Correlation coefficients \n of 1000 random shuffles of Temp", 
      xlab = "Correlation coefficients", 
      ylab = "Fequency",
      fill=I("gold"), 
      col=I("black")) + 
  geom_vline(xintercept = initial_cor, color = "red", size=1)
dev.off()

# is observed sig diff to random distibution
Pvalue <- length(test_year_1000 > initial_cor)/length(test_year_1000)
Pvalue

length(test_year_1000[abs(test_year_1000) > abs(cor(H[,1],H[,2]))])/length(test_year_1000)

