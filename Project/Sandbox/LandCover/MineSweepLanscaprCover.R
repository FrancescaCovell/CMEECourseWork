rm(list=ls())
MatDim <- c(11,11)
PerCov = c(0.25, 0.5, 0.75)

#### ####
#matrix(data = sample(c(1,0), replace=TRUE, size= MatDim[1] * MatDim[2] , prob = c(PerCov[2], (1-PerCov[2]))) ,  MatDim[1], ncol = MatDim[2])

#height <- MatDim[1]
#width <- MatDim[2]
for (i in 1:length(PerCov)) {
mines <- round(MatDim[1] * PerCov[i])

mine.index <- sample(MatDim[1] * MatDim[2], mines)
mine.mat <- matrix(0, MatDim[1], MatDim[2])
mine.mat[mine.index] <- -10

search.mine <- which(mine.mat < 0, arr.ind = TRUE)
mine.row <- search.mine[, 1]
mine.col <- search.mine[, 2]

for (i in 1:mines) {
  mrow <- intersect(1:MatDim[1], (mine.row[i] - 1):(mine.row[i] + 1))
  mcol <- intersect(1:MatDim[2], (mine.col[i] - 1):(mine.col[i] + 1))
  mine.mat[mrow, mcol] <- mine.mat[mrow, mcol] + 1
}

mine.mat <- ifelse(mine.mat < 0 , 1, mine.mat)
mine.mat <- ifelse(mine.mat > 1 , 1, mine.mat)

print(sum(mine.mat)/(MatDim[1]*MatDim[2]))

print("You Exploded")

}

#### ####
rm(list=ls())
MatDim <- c(11,11)
NumRun = 5
PerCov = c(0.25, 0.5, 0.75)
for (i in 1: NumRun) {
for (j in 1:length(PerCov)) {
  #set.seed(i)
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
  
  filename <-paste0("../Data/Fragments/MineSweeplandscape",i,"_", PerCov[j],".txt")
  write.table(mine.mat, file=filename, row.names=FALSE, col.names=FALSE)
}
  print("You Exploded")
}
