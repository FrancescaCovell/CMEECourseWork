#### Function ####

#CreateLandscapeCover <-function(MatDim = NULL, # Matrix dimensions
                         #       NumRun= NULL, # Number of runs, this is used to set seed for sampling
                          #      PerCov=NULL ){ # Percentage landscape covers
  # This function creates land scape comver matricies of size MatDim
  # NumRun is use to set seed and PerCov gives probabilities for sample
  # Output are sent to Data/Fragments/ and with the naming convention lanscapeSeed_PerCov.txt
  # for exaple landscape7_0.3.txt
  # Example: CreateLandscapeCover(MatDim = c(11,11), NumRun = 5, PerCov = c(0.25, 0.5, 0.75))
  # Will output 15 txt files each containing a matrix 11 by 11 filled based on respective probabilities
args <- commandArgs(TRUE)
MatDim<-eval(parse(text=args[1]))
NumRun<-eval(parse(text=args[2]))
PerCov<-eval(parse(text=args[3]))
  
  for (i in 1: NumRun) {
    for (j in 1:length(PerCov)) {
      set.seed(i)
      lanscape <-matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[j], (1-PerCov[j]))) ,  MatDim[1], ncol = MatDim[2])
      filename <-paste0("../Data/Fragments/landscape",i,"_", PerCov[j],".txt")
      write.table(lanscape, file=filename, row.names=FALSE, col.names=FALSE)
    }
  }
#}



#### Run function is test.sh ####

#CreateLandscapeCover(MatDim, NumRun, PerCov)
