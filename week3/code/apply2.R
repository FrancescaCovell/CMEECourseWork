

SomeOperation <- function(v){
  if (sum(v) > 0) { #if v is < 0 times by 100
    return(v * 100)
  }
  return(v)
}

M<- matrix(rnorm(100), nrow = 10, ncol = 10)# create randomly fill 10 by 10 matrix
print(apply(M, 1, SomeOperation)) # run function on matrix

