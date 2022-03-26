library(vegan)
library(tidyverse)
library(raster)
library(gtools)

#### test ####
a <- c(2,2,3,4,5)
b <- c(1,2,3,4,5)
c <- c(1,2,2,3,4)
df2<-data.frame(a,b,c)
vegdist(df2, method = "jaccard")

#### withing cell popspec ####

f<-read.table(file.choose())
f <- f[,colSums(f)!= 0]
h <- as.matrix(vegdist(f, method = "jaccard"))
jac <-vegdist(f, method = "jaccard")
jac_r <- raster(as.matrix(jac))
plot(jac_r, col=hcl.colors(20))



#### between landscape cover ####

f<-read.table(file.choose())
h <- which(f$V1 == 1976)
h
  


gpath = "/home/frcovell/Project/sandbox/Sumplimentary/Dispersal/Dispersal1"
setwd(gpath)
amalgamation_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "AmalgamationLandscape1_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()

for (i in 1: max(length(amalgamation_Paths))) {
  f <- read.table(paste0(amalgamation_Paths[i], "/totalPopSpec.txt"))
  h <- which(f$V1 == 1976)
}

seed = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "Seed_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()

test = str_split(seed, pattern = "/")[[1]][9] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)


for (i in 1: max(length(seed))) {
  g <- which(str_split(seed, pattern = "/")[[i]][9] == "Seed_1")
  
}
