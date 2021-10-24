

## build random matrix
M <- matrix(rnorm(100), nrow = 10, ncol = 10) 

## take the mean of each row
RowMeans <- apply(M, 1, mean)
print(RowMeans)


## Varience

RowVar <- apply(M, 1, var)
print(RowVar)

## by col
ColMeans <- apply(M, 2, mean)
print(ColMeans)

