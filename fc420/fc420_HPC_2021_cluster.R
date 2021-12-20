# CMEE 2021 HPC excercises R code HPC run code pro forma

rm(list=ls()) # good practice 
source("fc420_HPC_2021_main.R")

# read job number 
iter <-  as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

#  iter<- 10
  set.seed(iter)
 
  if (iter <= 25) {
    size = 500 
  }else if (iter <= 50) {
    size = 1000
  }else if (iter <= 75) {
    size = 2500
  }else if (iter <= 100) {
    size = 5000
  }

  cluster_run(speciation_rate = 0.0037588, size=size, wall_time=690, interval_rich=1,
              interval_oct=size/10, burn_in_generations=8*size,
              output_file_name= paste("my_test_file_", iter, ".rda"))
