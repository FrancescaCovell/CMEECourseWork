
#### Function ####

# a function to take a sample of sixe n from population
myexperiment <- function(popn, n) {
  pop_sample <- sample(popn, n, replace = F)
  return(mean(pop_sample))
  
}

#calculate mean using loop
loopy_sample1 <- function(popn, n, num){
 result1<- vector() # initialize empty vector, size 1
 for (i in 1:num) {
   result1 <- c(result1, myexperiment(popn, n))
 }
 return(result1)
}


#run num iterations of experiment usng for loop on vector with preallocation
loopy_sample2 <- function(popn, n, num) {
  result2 <- vector(,num) # preallocate expected size
  for (i in 1:num) {
    result2[i] <- myexperiment(popn, n)
  }
  return(result2)  
}

#run num iterations of experiment usng for loop on list with preallocation
loopy_sample3 <- function(popn, n, num){
  result3 <- vector("list", num) 
  for (i in 1:num) {
    result3[[i]] <- myexperiment(popn, n)
  }
  return(result3)
}

# run "num" iterations of the experiment usinf vectoriztion with lapply:
lapply_sample <- function(popn, n, num) {
  result4 <- lapply(1:num, function(i) myexperiment(popn, n))
  return(result4)
  
}

# run "num" iterations of the experiment usinf vectoriztion with sapply:
sapply_sample <- function(popn, n, num){
  result5 <- sapply(1:num, function(i) myexperiment(popn, n))
  return(result5)
} 

# setting perametre
set.seed(12345)
popn <- rnorm(10000) # generate population
hist(popn)

n <- 100 # sample size for each experiment 
num <- 10000# number of times to rerun experiment 

print("Using loops without preallocation on a vector took:" )
print(system.time(loopy_sample1(popn, n, num)))

print("Using loops with preallocation on a vector took:" )
print(system.time(loopy_sample2(popn, n, num)))

print("Using loops with preallocation on a list took:" )
print(system.time(loopy_sample3(popn, n, num)))

print("Using the vectorized sapply function (on a list) took:" )
print(system.time(sapply_sample(popn, n, num)))

print("Using the vectorized lapply function (on a list) took:" )
print(system.time(lapply_sample(popn, n, num)))


#### Using by ####

attach(iris)

#colmean
by(iris[,1:2], iris$Species, colMeans)

by(iris[,1:2], iris$Petal.Width, colMeans)


#### using replicate ####

replicate(10, runif(5))

