##### V1.3 CreateLandscapeCover ####

#### Random landscape cover ####
RandomLandscapeCover <-function(MatDim = NULL, # Matrix dimensions
                                NumRun= NULL, # Number of runs, this is used to set seed for sampling
                                PerCov=NULL ){ # Percentage landscape covers
  # This function creates randomly filled landscape cover matricies of size MatDim
  # NumRun is use to set seed and PerCov gives probabilities for sample
  # Output are sent to Data/Fragments/ and with the naming convention RandomLanscapeSeed_PerCov.txt
  # for example RandomLandscape7_0.3.txt
  # Example: 
  # input: RandomLandscapeCover(MatDim = c(11,11), NumRun = 5, PerCov = c(0.25, 0.5, 0.75))
  # output: 15 txt files each containing a matrix 11 by 11 filled based on respective probabilities
  
  for (i in 1: NumRun) {
    for (j in 1:length(PerCov)) {
      set.seed(i)
      lanscape <-matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[j], (1-PerCov[j]))) ,  MatDim[1], ncol = MatDim[2])
      filename <-paste0("../../Data/Fragments/RandomLandscape",i,"_", PerCov[j],".txt")
      write.table(lanscape, file=filename, row.names=FALSE, col.names=FALSE)
    }
  }
}




#### Amalgamation landscape cover ####
AmalgamationLandscapeCover <-function(MatDim = NULL, # Matrix dimensions
                                      NumRun= NULL, # Number of runs, this is used to set seed for sampling
                                      PerCov=NULL ){ # Percentage landscape covers
  # This function creates an weighted landscape cover matricies of size MatDim
  # Based on mine_sweeper (yihui/fun: Use R for Fun)
  # NumRun is use to set seed and PerCov gives probabilities for sample
  # Output are sent to Data/Fragments/ and with the naming convention RandomLanscapeSeed_PerCov.txt
  # for example AmalgamationLandscape7_0.3.txt
  # Example: 
  # input: AmalgamationLandscapeCover(MatDim = c(11,11), NumRun = 5, PerCov = c(0.25, 0.5, 0.75))
  # output: 15 txt files each containing a matrix 11 by 11 filled based on respective probabilities
  for (i in 1: NumRun) {
    for (j in 1:length(PerCov)) {
      set.seed(i)
      
      #determine number of central points based of PerCov
      LandscapeCent <- round((2*MatDim[1]) * PerCov[j]) 
      
      #Random placement of central points
      Land.index <- sample(MatDim[1] * MatDim[2], LandscapeCent) 
      #initialize land matrix
      Land.mat <- matrix(0, MatDim[1], MatDim[2]) 
      #identify central point on land matrix
      Land.mat[Land.index] <- -10 
      
      #coordinates for central points
      search.Cent <- which(Land.mat < 0, arr.ind = TRUE) 
      Land.row <- search.Cent[, 1]
      Land.col <- search.Cent[, 2]
      
      for (k in 1:LandscapeCent) {
        #identify Rows and Col coordinates next to LandscapeCent
        mrow <- intersect(1:MatDim[1], (Land.row[k] - 1):(Land.row[k] + 1))
        mcol <- intersect(1:MatDim[2], (Land.col[k] - 1):(Land.col[k] + 1))
        #increase value of identifie points by 1
        Land.mat[mrow, mcol] <- Land.mat[mrow, mcol] + 1
      }
      
      # if points in matix not equal to 0 make equal 1 to identify as forest 
      Land.mat <- ifelse(Land.mat !=0 , 1, Land.mat)
      
      
      filename <-paste0("../../Data/Fragments/AmalgamationLandscape",i,"_", PerCov[j],".txt")
      write.table( Land.mat, file=filename, row.names=FALSE, col.names=FALSE)
    }
  }
}

#### Raster landscape cover ####
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
