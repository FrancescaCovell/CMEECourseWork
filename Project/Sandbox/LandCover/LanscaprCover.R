#### build area ####
rm(list=ls())
MatDim <- c(10,10)
PerCov = c(0, 0.25, 0.5, 0.75, 1)

# changing pros
set.seed(11)
#PC0<- matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[1], (1-PerCov[1]))) ,  MatDim[1], ncol = MatDim[2])
#set.seed(11)
PC25<- matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[2], (1-PerCov[2]))) ,  MatDim[1], ncol = MatDim[2])
sum(PC25)
set.seed(11)
PC5<- matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[3], (1-PerCov[3]))) ,  MatDim[1], ncol = MatDim[2])
set.seed(11)
PC75<- matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[4], (1-PerCov[4]))) ,  MatDim[1], ncol = MatDim[2])
set.seed(11)
test<- matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c((1-PerCov[2]),PerCov[2])) ,  MatDim[1], ncol = MatDim[2])
set.seed(11)
#set.seed(11)
#PC1<- matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[5], (1-PerCov[5]))) ,  MatDim[1], ncol = MatDim[2])

for (i in 1:length(PerCov)) {
  set.seed(11)
  assign(paste0("PerCov_", PerCov[i]), matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[i], (1-PerCov[i]))) ,  MatDim[1], ncol = MatDim[2]))
}

#multi run
rm(list=ls())
MatDim <- c(11,11)
NumRun = 5
PerCov = c(0, 0.25, 0.5, 0.75, 1)
for (i in 1: NumRun) {
  for (j in 1:length(PerCov)) {
  set.seed(i)
  h <- matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[j], (1-PerCov[j]))) ,  MatDim[1], ncol = MatDim[2]) #assign(paste0("PerCov_",i,"_", PerCov[j]),
  filename <-paste0("../Data/Fragments/landscape",i,"_", PerCov[j],".txt")
  write.table(h, file=filename, row.names=FALSE, col.names=FALSE)}
}
#while (NumRun > 0) {
 # for (j in 1:length(PerCov)) {
#    set.seed(NumRun)
#    assign(paste0("PerCov_",NumRun,"_", PerCov[j]), matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[j], (1-PerCov[j]))) ,  MatDim[1], ncol = MatDim[2]))
#  }
#  NumRun <- NumRun -1
#}
#}
#### final function ####
rm(list=ls())

CreateLandscapeCover <-function(MatDim = NULL, 
                               NumRun= NULL, 
                               PerCov=NULL ){
  for (i in 1: NumRun) {
    for (j in 1:length(PerCov)) {
      set.seed(i)
      lanscape <-matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[j], (1-PerCov[j]))) ,  MatDim[1], ncol = MatDim[2])
      filename <-paste0("../Data/Fragments/landscape",i,"_", PerCov[j],".txt")
      write.table(lanscape, file=filename, row.names=FALSE, col.names=FALSE)
    }
  }
}

           