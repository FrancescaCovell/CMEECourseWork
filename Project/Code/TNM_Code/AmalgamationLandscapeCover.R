#### V1.1 Amalgamation landscape cover ####

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
      mines <- round((2*MatDim[1]) * PerCov[j])
      
      mine.index <- sample(MatDim[1] * MatDim[2], mines)
      mine.mat <- matrix(0, MatDim[1], MatDim[2])
      mine.mat[mine.index] <- -10
      
      search.mine <- which(mine.mat < 0, arr.ind = TRUE)
      mine.row <- search.mine[, 1]
      mine.col <- search.mine[, 2]
      
      for (k in 1:mines) {
        mrow <- intersect(1:MatDim[1], (mine.row[k] - 1):(mine.row[k] + 1))
        mcol <- intersect(1:MatDim[2], (mine.col[k] - 1):(mine.col[k] + 1))
        mine.mat[mrow, mcol] <- mine.mat[mrow, mcol] + 1
      }
      
      mine.mat <- ifelse(mine.mat < 0 , 1, mine.mat)
      mine.mat <- ifelse(mine.mat > 1 , 1, mine.mat)
      
      print(sum(mine.mat)/(MatDim[1]*MatDim[2]))
      
      filename <-paste0("../../Data/Fragments/AmalgamationLandscape",i,"_", PerCov[j],".txt")
      write.table(mine.mat, file=filename, row.names=FALSE, col.names=FALSE)
    }
  }
  print("You Exploded")
}
