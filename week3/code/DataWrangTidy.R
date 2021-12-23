###############################################################
################## Wrangling the Pound Hill Dataset ############
################################################################
rm(list=ls())
require(tidyverse)

############# Load the dataset ###############
# header = false because the raw data don't have real headers
MyData<- as.matrix(read.csv("../data/PoundHillData.csv", header = FALSE))

# header = true because we do have metadata headers
MyMetaData <- read.csv("../data/PoundHillMetaData.csv", header = TRUE, sep = ";")

############# Inspect the dataset ###############
head(MyData)
dim(MyData)
str(MyData)
fix(MyData) #you can also do this
fix(MyMetaData)


############# Transpose ###############
# To get those species into columns and treatments into rows 
MyData <- t(MyData) 
#head(MyData)
#dim(MyData)


#tidy transposed data from MyData <- t(MyData)
MyData<-as.data.frame(MyData)

colnames(MyData) <- MyData[1,]
MyData <-MyData[-c(1),]




############# Replace species absences with zeros ###############
#MyData[MyData == ""] = 0

############# Convert raw matrix to data frame ###############

#TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F) #stringsAsFactors = F is important!
#colnames(TempData) <- MyData[1,] # assign column names from original data

############# Convert from wide to long format  ###############
#require(reshape2) # load the reshape2 package

#?melt #check out the melt function
mutate

#MyWrangledData <- gather(MyData, "Cultivation", "Block", "Plot", "Quadrat", "Species", "Count")
MyWrangledData <-  pivot_longer(MyData, cols = 5:45, names_to = "Species" , values_to = "Count")  

MyWrangledData <- MyWrangledData %>% # into MyWrang put MyWrang do following
  mutate(across(c(Cultivation, Block, Plot, Quadrat), as.factor))

MyWrangledData <- MyWrangledData %>% # into MyWrang put MyWrang do following
  mutate(across(c(Count), as.integer))

#MyWrangledData[, "Cultivation"] <- as.factor(MyWrangledData[, "Cultivation"])
#MyWrangledData[, "Block"] <- as.factor(MyWrangledData[, "Block"])
#MyWrangledData[, "Plot"] <- as.factor(MyWrangledData[, "Plot"])
#MyWrangledData[, "Quadrat"] <- as.factor(MyWrangledData[, "Quadrat"])
#MyWrangledData[, "Count"] <- as.integer(MyWrangledData[, "Count"])

str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData)

