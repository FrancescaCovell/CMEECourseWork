##### V1.1 RasterLandscapeCover ####

#load libraries 
library(raster)
library(rgdal)
#setwd("Project/Code/TNM_Code/")

#import and check raster
init_land <- raster(paste0("../../Data/hansen_image_PID0006.tif"))
#col=terrain.colors(2)
#brk <- c(69, 100) # break color based on forest >69
#plot(init_land, col=col, breaks=brk, main="DEM with more breaks")
#res(init_land)
#dim(init_land)

#scale down to 11 by 11
scale_land <- aggregate(init_land, fact=c(ceiling(dim(init_land)[2]/11), ceiling(dim(init_land)[1]/11)))
res(scale_land)
dim(scale_land)

#col=terrain.colors(2)
#brk <- c(69, 100) # break color based on forest >69
#plot(test.aggregate, col=col, breaks=brk, main="DEM with more breaks")

#convert to matix
Land_Mat<- as.matrix(scale_land)
dim(Land_Mat)

Land_Mat <- ifelse(Land_Mat  > 69 , 100, Land_Mat )
Land_Mat <- ifelse(Land_Mat  < 100 , 0, Land_Mat )
paste0("Percentage Landscape Cover = ", sum(Land_Mat)/length(Land_Mat))

#output matix as txt
#filename <-paste0("./landscape_hansen_image_PID1011.txt")
#write.table(test2, file=filename, row.names=FALSE, col.names=FALSE)

#output matric as new tif
height <- 11
width <-11
tif_driver <- new("GDALDriver", "GTiff")
tif <- new("GDALTransientDataset", tif_driver, height, width, 1, 'Byte')

#pattern <- matrix(test2, width, height)
#pattern <- ifelse(pattern  > 69 , 100, pattern )
#pattern <- ifelse(pattern  < 70 , 100, pattern )
#pattern <- ifelse(pattern  < 100 , 0, pattern )
#pattern <- ifelse(pattern  > 0 , 1, pattern )
bnd1 <- putRasterData(tif, Land_Mat)
tif_file <- "../../Data/result06.tif"
saveDataset(tif, tif_file)
GDAL.close(tif)
GDAL.close(tif_driver)

#output matix as txt
Land_txt <- ifelse(Land_Mat  == 100 , 1, Land_Mat )
filename <-paste0("../../Data/landscape_hansen_image_PID0006.txt")
write.table(Land_txt, file=filename, row.names=FALSE, col.names=FALSE)
