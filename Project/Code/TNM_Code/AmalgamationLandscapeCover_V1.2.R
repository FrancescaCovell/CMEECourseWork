#### V1.2 Amalgamation landscape cover ####
#Change Variable names, added comments and removed unnecessary code 

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
      write.table(mine.mat, file=filename, row.names=FALSE, col.names=FALSE)
    }
  }
  print("You Exploded")
}
