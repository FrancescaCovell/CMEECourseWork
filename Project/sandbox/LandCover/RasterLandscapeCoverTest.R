library(raster)
library(rgdal)
setwd("Project/sandbox/")

test <- raster(paste0("./hansen_image_PID0006.tif"))
col=terrain.colors(2)
brk <- c(69, 100) # break color based on forest >69
plot(test, col=col, breaks=brk, main="DEM with more breaks")
res(test)
dim(test)

mergecol<-ceiling(dim(test)[2]/4)
mergerow<-ceiling(dim(test)[2]/4)
testmerge.aggregate <- aggregate(test, fact=c(ceiling(dim(test)[2]/mergecol), ceiling(dim(test)[1]/mergerow)))

col=terrain.colors(2)
brk <- c(69, 100) # break color based on forest >69
plot(testmerge.aggregate, col=col, breaks=brk, main="DEM with more breaks")

test.aggregate <- aggregate(testmerge.aggregate, fact=c(ceiling(dim(testmerge.aggregate)[2]/11), ceiling(dim(testmerge.aggregate)[1]/11)))
res(test.aggregate)
dim(test.aggregate)


col=terrain.colors(2)
brk <- c(69, 100) # break color based on forest >69
plot(test.aggregate, col=col, breaks=brk, main="DEM with more breaks")


test2<- as.matrix(test.aggregate)
test2 <- ifelse(test2  > 69 , 100, test2 )
test2 <- ifelse(test2  < 100 , 0, test2 )
sum(test2)/length(test2)
test70 <- read.table("../Data/Fragments/AmalgamationLandscape1_0.7.txt")

test70 <- ifelse(test70  == 1 , 100, test2 )
sum(test70)/length(test70)

dim(test2)

height <- 11
width <-11
tif_driver <- new("GDALDriver", "GTiff")
tif <- new("GDALTransientDataset", tif_driver, height, width, 1, 'Byte')

pattern <- matrix(test2, width, height)
pattern <- ifelse(pattern  > 69 , 100, pattern )
#pattern <- ifelse(pattern  < 70 , 100, pattern )
pattern <- ifelse(pattern  < 100 , 0, pattern )
#pattern <- ifelse(pattern  > 0 , 1, pattern )
bnd1 <- putRasterData(tif, pattern)
tif_file <- "result11.tif"
saveDataset(tif, tif_file)
GDAL.close(tif)
GDAL.close(tif_driver)
