rm(list=ls())

#set.seed(10)
width <- 11
height <- 11
mines <- 15

m <- rep(0, width * height)
# Status: 0 for untested areas, 1 for tested areas, 2 for flags
mat.status <- matrix(m, height, width)
mine.index <- sample(width * height, mines)
m[mine.index] <- -10
mine.mat <- matrix(m, height, width)
search.mine <- which(mine.mat < 0, arr.ind = TRUE)
mine.row <- search.mine[, 1]
mine.col <- search.mine[, 2]
# Calculate the number of mines in every 3x3 square
for (i in 1:mines) {
  mrow <- intersect(1:height, (mine.row[i] - 1):(mine.row[i] + 1))
  mcol <- intersect(1:width, (mine.col[i] - 1):(mine.col[i] + 1))
  mine.mat[mrow, mcol] <- mine.mat[mrow, mcol] + 1
}
mine.mat <- ifelse(mine.mat < 0 , 1, mine.mat)
mine.mat <- ifelse(mine.mat > 1 , 1, mine.mat)

sum(mine.mat)/(width*height)

print("You Exploded")
