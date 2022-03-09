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
CreateMat = function(res_Path, res) {
  data = read.table(file = paste0(res_Path, res)
}
  


gpath = "/home/frcovell/Project/sandbox/Perliminary/Par test results/Disp and interact change/Dist1Inter1"
setwd(gpath)
amalgamation_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "AmalgamationLandscape1_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()




