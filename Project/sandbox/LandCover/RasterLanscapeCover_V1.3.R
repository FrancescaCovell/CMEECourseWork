##### V1.3 RasterLandscapeCover ####
# merged cell so each cell ~120m before scaling

RasterLandscapeCover <- function(RasterPath = NULL, # Path to Raster to be converted
                                 TifOut = NULL, # Path for .tif output
                                 TxtOut = NULL,# Path for .txt output
                                 MatDim = NULL) { # Matrix Dimension
  
  # This function creates landscape cover matrices from a raster of size MatDim
  # RasterPath give the path to a tif file to be imported as a Raster
  # Output are sent to TifOut and TxtOut 
  # Example: 
  # input: RasterLandscapeCover(RasterPath = "../../Data/hansen_image_PID0006.tif", TifOut = "../../Data/result06.tif",
  # TxtOut = "../../Data/landscape_hansen_image_PID0006.txt", MatDim = c(11,11))
  # output: result06.tif, landscape_hansen_image_PID0006.txt
  # in terminal pasts "Percentage Cover equals:29.7520661157025"
  
  #load libraries 
  require(raster)
  require(rgdal)
  
  #import raster
  init_land <- raster(paste0(RasterPath))
  
  #scale down 1 cell = 120 m
  #each cell ~30m by merging 4 cell each cell~120m
  mergecol<-ceiling(dim(init_land)[2]/4)
  mergerow<-ceiling(dim(init_land)[2]/4)
  merg_land <- aggregate(init_land, fact=c(ceiling(dim(init_land)[2]/mergecol), ceiling(dim(init_land)[1]/mergerow)))
  
  #scale down to 11 by 11
  scale_land <- aggregate( merg_land, fact=c(ceiling(dim( merg_land)[2]/11), ceiling(dim( merg_land)[1]/11)))
  res(scale_land)
  dim(scale_land)
  
  #convert to matix
  Land_Mat<- as.matrix(scale_land)
  
  #fill with 100 and 0 and work out  percentage cover
  Land_Mat <- ifelse(Land_Mat  > 69 , 100, Land_Mat )
  Land_Mat <- ifelse(Land_Mat  < 100 , 0, Land_Mat )
  PerCov <- sum(Land_Mat)/length(Land_Mat)

  #output matric as new tif
  height <- MatDim[1]
  width <-MatDim[2]
  tif_driver <- new("GDALDriver", "GTiff")
  tif <- new("GDALTransientDataset", tif_driver, height, width, 1, 'Byte')

  bnd1 <- putRasterData(tif, Land_Mat)
  tif_file <-TifOut
  saveDataset(tif, tif_file)
  GDAL.close(tif)
  GDAL.close(tif_driver)
  
  #output matix as txt
  Land_txt <- ifelse(Land_Mat  == 100 , 1, Land_Mat )
  filename <-paste0(TxtOut)
  write.table(Land_txt, file=filename, row.names=FALSE, col.names=FALSE)
  
  paste0("Percentage Cover equals:", PerCov)
}
