
rm(list=ls())
getwd()

require(plyr)

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")
dim(MyDF) #check the size of the data frame you loaded
dplyr::glimpse(MyDF)


# draw and save 3 figs
#subplot distribution Predator mass
#subplot distribution Prey mass
#size ratio of prey mass over predator mass by feeding interaction type
#Use logarithms of masses (or size ratios)for all three plots.
MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] <- MyDF$Prey.mass[which(MyDF$Prey.mass.unit == 'mg')] / 1000

x <- split(log(MyDF$Predator.mass), f = MyDF$Type.of.feeding.interaction)
y <- split(log(MyDF$Prey.mass), f = MyDF$Type.of.feeding.interaction)
length(x)

#subplot for predator
pdf("../results/Pred_Subplots.pdf", # Open blank pdf page using a relative path
    11.7, 8.3)
par(mfcol = c(5, 1))
par(mfg = c(1, 1))
par(mar =c(1,1,1,1))
hist(x$insectivorous, 
     xlab = "log 10 (Predator mass (g)) (g))", ylab = "count",
     col = "blue", main = "Predator mass for insectivorous")
par(mfg = c(2, 1))
hist(x$piscivorous, 
     xlab = "log 10 (Predator mass (g))", ylab = "count",
     col = "red", main = "Predator for piscivorous")
par(mfg = c(3, 1))
hist(x$planktivorous, 
     xlab = "log 10 (Predator mass (g))", ylab = "count",
     col = "gold", main = "Predator mass planktivorous")
par(mfg = c(4, 1))
hist(x$predacious, 
     xlab = "log 10 (Predator mass (g))", ylab = "count",
     col = "green", main = "Predator mass predacious")
par(mfg = c(5, 1))
hist(x$`predacious/piscivorous`, 
     xlab = "log 10 (Predator mass (g))", ylab = "count",
     col = "purple", main = "Predator mass for predacious/piscivorous")
dev.off()

#works 

#subplot for prey
pdf("../results/Prey_Subplots.pdf", # Open blank pdf page using a relative path
    11.7, 8.3)
par(mfcol = c(5, 1))
par(mfg = c(1, 1))
par(mar =c(1,1,1,1))
hist(y$insectivorous, 
     xlab = "log 10 (Prey mass (g)) (g))", ylab = "count",
     col = "blue", main = "Prey mass for insectivorous")
par(mfg = c(2, 1))
hist(y$piscivorous, 
     xlab = "log 10 (Prey mass (g))", ylab = "count",
     col = "red", main = "Prey for piscivorous")
par(mfg = c(3, 1))
hist(y$planktivorous, 
     xlab = "log 10 (Prey mass (g))", ylab = "count",
     col = "gold", main = "Prey mass planktivorous")
par(mfg = c(4, 1))
hist(y$predacious, 
     xlab = "log 10 (Prey mass (g))", ylab = "count",
     col = "green", main = "Prey mass predacious")
par(mfg = c(5, 1))
hist(y$`predacious/piscivorous`, 
     xlab = "log 10 (Prey mass (g))", ylab = "count",
     col = "purple", main = "Prey mass for predacious/piscivorous")
dev.off()
#work

# subplots for size ratio

z <- log(MyDF$Prey.mass / MyDF$Predator.mass)
z <- split(z, f = MyDF$Type.of.feeding.interaction)
length(z)
pdf("../results/SizerRatio_Subplots.pdf", # Open blank pdf page using a relative path
    11.7, 8.3)
par(mfcol = c(5, 1))
par(mfg = c(1, 1))
par(mar =c(1,1,1,1))
hist((z$insectivorous), 
     xlab = "log Prey:Predator size ratio)", ylab = "count",
     col = "blue", main = "Prey:Predator size ratio for insectivorous")
par(mfg = c(2, 1))
hist((z$piscivorous), 
     xlab = "log Prey:Predator size ratio)", ylab = "count",
     col = "red", main = "Prey:Predator size ratio for piscivorous")
par(mfg = c(3, 1))
hist((z$planktivorous), 
     xlab = "log (Prey:Predator size ratio)", ylab = "count",
     col = "gold", main = "Prey:Predator size ratio mass planktivorous")
par(mfg = c(4, 1))
hist((z$predacious), 
     xlab = "log (Prey:Predator size ratio)", ylab = "count",
     col = "green", main = "Prey:Predator size ratio mass predacious")
par(mfg = c(5, 1))
hist((z$`predacious/piscivorous`), 
     xlab = "log (Prey:Predator size ratio)", ylab = "count",
     col = "purple", main = "Prey:Predator size ratio for predacious/piscivorous")
dev.off()


#calculate the (log) mean and median predator mass, prey mass 
#predator-prey size-ratios 
#to a csv file.
#and a single csv file PP_Results.csv containing 
#the mean and median log predator mass, prey mass, and predator-prey size ratio

PP_Results <- data.frame(MeanPred = sapply(x, mean),
                   MedPred = sapply(x, median),
                   MeanPrey = sapply(y, mean),
                   MedPrey = sapply(y, mean),
                   MeanRatio = sapply(z, mean),
                   MedRatio = sapply(z, median))

MeanPred = sapply(x, mean)
MedPred = sapply(x, median)
MeanPrey = sapply(y, mean)
MedPrey = sapply(y, mean)
MeanRatio = sapply(z, mean)
MedRatio = sapply(z, median)
Mean<- c(MeanPred, MeanPrey, MeanRatio)
Median<-c(MedPred, MedPrey, MedRatio)
FeedingType <- rep(c(sort(unique(MyDF$Type.of.feeding.interaction))), 3)
Type <- rep(c("Pred","Prey","Ratio"), each = 5)

PP_Results <- data.frame(FeedingType,Type,Mean,Median)

write.csv( PP_Results, "../results/ PP_Results.csv")

                   