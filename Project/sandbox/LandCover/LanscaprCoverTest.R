#rm(list=ls())
#MatDim <- c(2,11) #Matix Dimesion
#PerCov = c(0, 0.25, 0.5, 0.75, 1) #Percentage Covers

#set.seed(11)
#PC25<- sample(c(1,0), replace=TRUE, size= 22 , prob = c(PerCov[2], (1-PerCov[2])))
#set.seed(11)
#PC5<- sample(c(1,0), replace=TRUE, size= 22 , prob = c(PerCov[3], (1-PerCov[3]))) 
#set.seed(11)
#PC75<-sample(c(1,0), replace=TRUE, size= 22 , prob = c(PerCov[4], (1-PerCov[4])))

#for (i in 1:length(PerCov)) {
#  set.seed(11)
#  assign(paste0("PerCov_", PerCov[i]), matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[i], (1-PerCov[i]))) ,  MatDim[1], ncol = MatDim[2]))
#}

arg <- commandArgs(trailingOnly = TRUE)
MatDim <- arg[1]
NumRun <- arg[2]
PerCov <- arg[3]
           
for (i in 1: NumRun) {
  for (j in 1:length(PerCov)) {
    set.seed(i)
    lanscape <-matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[j], (1-PerCov[j]))) ,  MatDim[1], ncol = MatDim[2])
    filename <-paste0("../Data/Fragments/landscape",i,"_", PerCov[j],".txt")
    write.table(lanscape, file=filename, row.names=FALSE, col.names=FALSE)
  }
}
