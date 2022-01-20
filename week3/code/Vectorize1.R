
M <- matrix(runif(1000000),1000,1000) #create matrix

SumAllElements <- function(M){
  Dimensions <- dim(M) # put dimension of matrix into Dimensions
  Tot <- 0
  for (i in 1:Dimensions[1]){
    for (j in 1:Dimensions[2]){
      Tot <- Tot + M[i,j] # loops though Dimension iterations and sums all elements in matrix
    }
  }
  return (Tot)
}

print("Using loops, the time taken is:")
print(system.time(SumAllElements(M)))

print("Using the in-built vectorized function, the time taken is:")
print(system.time(sum(M)))
