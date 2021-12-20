# CMEE 2021 HPC excercises R code main pro forma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Francesca Covell"
preferred_name <- "Fran"
email <- "f.covell20@imperial.ac.uk"
username <- "fc420"

# please remember *not* to clear the workspace here, or anywhere in this file. If you do, it'll wipe out your username information that you entered just above, and when you use this file as a 'toolbox' as intended it'll also wipe away everything you're doing outside of the toolbox.  For example, it would wipe away any automarking code that may be running and that would be annoying!
require(ggplot2)
# Question 1
species_richness <- function(community){
  return(length(unique(community))) #finds every unique value in community and returns the total length 
}

# Question 2
init_community_max <- function(size){
  return(seq(size)) #generate an initial community with the maximum possible number of species based on size
}

# Question 3
init_community_min <- function(size){
  return(rep(1,size)) #generate an initial community with the minimum possible number of species based on size
}


# Question 4
choose_two <- function(max_value){
  return(sample(max_value, 2, replace = F)) #chooses two vaules between 0 and max_value without replacement
  
}

# Question 5
neutral_step <- function(community){
  chosen <- choose_two(length(community)) # chooses 2 individual from community
  community[chosen[1]] <- community[chosen[2]] # replace one with the other to represent death and birth
  return(community)
}

# Question 6
neutral_generation <- function(community){
  
  if (length(community) %% 2 != 0){ #if community length is odd
    steps <- (length(community)/2) + sample(c(0.5, -0.5), 1) #randomly round steps up or down and caulculate number of steps
  }else{
    steps <- (length(community)/2) # caulculate number of steps
  }
  
  for (i in 1:steps) {
    community <- neutral_step(community) #run neutral_steps for number of calculated steps 
   
  }
  return(community) 
}

# Question 7
neutral_time_series <- function(community,duration)  {
  SpecieRichness <- species_richness(community) # work out start species richness
  for (i in 1:duration) { # duration mimic number of generations to be simulated
   community <- neutral_generation(community) # change the community composition after generation
    SpecieRichness <- append(SpecieRichness, species_richness(community)) # store species richness at every generation
  }
  
  return(SpecieRichness)
  
}

# Question 8
question_8 <- function() {
  graphics.off()# clear any existing graphs and plot your graph within the R window
  df<- data.frame(SpeciesRichness = neutral_time_series(init_community_max(100), 200),
                 Time = c(0:200))#Run simulation with set parameters 
  #plot results from simulation
  Graph8<- ggplot(df, aes(x = Time, y = SpeciesRichness))+
    geom_line(color="red")+
    xlab("Generation")+
    ylab("Species Richness")+
    ggtitle("Plot of Species Richness Over Generation")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #remove grid lines
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    print(Graph8)
  return("The state of a system will allways convergue to 1 species if left long enought, because within the simulation there is extintion but no spiciation so the model can only loose species")
}

#time?


# Question 9
neutral_step_speciation <- function(community,speciation_rate)  {
    chosen <- choose_two(length(community)) # choose two individuals from community
    spiceiation <- runif(1, 0, 1) <= speciation_rate # check is speciation has occurred
    if (spiceiation == TRUE) {
      community[chosen[1]] <- max(community) + 1 # if specieation has occurred create new species by adding 1 to the max community
    }
    else{
      community[chosen[1]] <- community[chosen[2]]# if o speciation occurred simulate death nd reproduction
    }
    return(community)
  }

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  
  if (length(community) %% 2 != 0){
    steps <- (length(community)/2) + sample(c(0.5, -0.5), 1)
  }else{
    steps <- (length(community)/2)
  }
  
  for (i in 1:steps) {
    community <- neutral_step_speciation(community, speciation_rate)#run neutral_steps_speciation for number of calculated steps
    
  }
  return(community) 
}


# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  SpecieRichness <- species_richness(community)
  for (i in 1:duration) {
    community <- neutral_generation_speciation(community, speciation_rate)
    SpecieRichness <- append(SpecieRichness, species_richness(community))
  }
  
  return(SpecieRichness)
  
}

# Question 12
question_12 <- function()  {
  graphics.off() # clear any existing graphs and plot your graph within the R window
  MaxCommunitySim<-neutral_time_series_speciation(init_community_max(100),0.1, 200) #simulate change in species richness over generation for max initial community
  MinCommunitySim<-neutral_time_series_speciation(init_community_min(100),0.1,200)  #simulate change in species richness over generation for min initial community
  plot.ts(cbind(MaxCommunitySim, MinCommunitySim), plot.type = "single", col = c("red", "blue"),
          xlab = "Generation", ylab = "Species Richness", 
          main = "Spcies Richness Over Generation based on \n Minimum and Maximum Inital Condition")
  legend("topright", legend = c("Max Initial No.Species", "Min Initial No.Species"), col = c("red", "blue"),  lwd = 1)
  return("Whether the initial species richness is high or low when there is speciation and extintion the species richness will reach a dynamic equilibrium.")
}


# Question 13
species_abundance <- function(community)  {
  abundance <-c(sort(table(community), decreasing = TRUE))# sort contingency table of the counts in decreasing order
  names(abundance)<-NULL #removecolunm names of table 

    return(abundance)
  }

# Question 14
octaves <- function(abundance_vector) {
  AbundanceBin <- tabulate(bin = floor(log2(abundance_vector))+1)
  #tabulate(counts the number of times each integer occurs) in bins= the largest integers not greater than log2(abundance_vector))+1 
  return(AbundanceBin)
}

# Question 15
# accept 2 values return the sum 
sum_vect <- function(x, y) {
  if (length(y) > length(x)) {
    x <-append(x, rep(0, (length(y) - length(x)))) # if y is bigger the x, fill x with 0 to make up the difference
  }else if (length(x) > length(y)) {
    y <-append(y, rep(0, (length(x) - length(y))))# if x is bigger the y, fill y with 0 to make up the difference
    
  }
  return(x+y)
}

# Question 16 
# run neutal model for 200 gen record spp abundance octave vector
question_16 <- function()  {
  graphics.off() # clear any existing graphs and plot your graph within the R window
  #run initial neutral model with speciation for max and min initial community
  CommunityMax<-neutral_generation_speciation(init_community_max(100),0.1)
  CommunityMin<-neutral_generation_speciation(init_community_min(100),0.1)
  #run neutral model with speciation for 200 generations to simulate burn in
  for (i in 1:200) {
    CommunityMax<-neutral_generation_speciation(CommunityMax,0.1)
    CommunityMin<-neutral_generation_speciation(CommunityMin, 0.1)
  }
  #record species abundance after burn in
  AbundanceMax <- list(octaves(species_abundance(CommunityMax)))
  AbundanceMin <- list(octaves(species_abundance(CommunityMin)))
  
  #run neutral model with speciation for 2000 generations and record abundance count
  for (i in 1:2000) {
    CommunityMax<-neutral_generation_speciation(CommunityMax,0.1)
    CommunityMin<-neutral_generation_speciation(CommunityMin,0.1)
    if (i %% 20 == 0) {
      TempAbundanceMax <- octaves(species_abundance(CommunityMax))
      AbundanceMax[[length(AbundanceMax)+1]] <- TempAbundanceMax
      TempAbundanceMin <- octaves(species_abundance(CommunityMin))
      AbundanceMin[[length(AbundanceMin)+1]] <- TempAbundanceMin
      }
  }
  SumAbundanceMax<-sum_vect(AbundanceMax[[1]], AbundanceMax[[2]])
  SumAbundanceMin<-sum_vect(AbundanceMin[[1]], AbundanceMin[[2]])
  for (i in 3:length(AbundanceMax)) {
    SumAbundanceMax <- sum_vect(SumAbundanceMax, AbundanceMax[[i]])
    SumAbundanceMin <- sum_vect(SumAbundanceMin, AbundanceMin[[i]])
  }
  MeanAbundance <- data.frame(SppCount = c(SumAbundanceMax/length(AbundanceMax),SumAbundanceMin/length(AbundanceMin)), 
                              condition = c(rep("Init_Community_Max", length(SumAbundanceMax)),rep("Init_Community_Min", length(SumAbundanceMin))),
                              frequ = c("1","2-3","4-7","8-15","16-31","32-63"))
  Graph16 <- ggplot(MeanAbundance, aes(fill=condition, y = SppCount, x = frequ))+
    geom_bar(position = "dodge", stat = "identity")+
    ggtitle("Plot of Species Abundance Count")+
    xlab("Number Individuals per Species Count")+
    ylab("Species Count")+
    labs(fill = "Initial Community Condition")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.justification = "top")+
    scale_x_discrete(limit = c("1","2-3","4-7","8-15","16-31","32-63"))+
  scale_fill_discrete(labels = c("Maximum spicies","Minimum spicies"))
  print(Graph16)
  
  return("The initial condition of maximum or minimum number of species based on community size is not shown to make a significant difference. This neutral theory assumes demographic properties of the individual are independant of its species identity")
}

# Question 17

cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
  ptm <- proc.time() #start timer
  #initialise start values
  CommunityMin <- neutral_generation_speciation(init_community_min(size), speciation_rate)
  RichnessMin <- list(c(species_richness(CommunityMin))) 
  AbundanceMin <- c()
  Generation <- 1
  while((proc.time() - ptm)[3] < (wall_time * 60)){ # stop when timer reaches wall_time in minutes 
    Generation = Generation + 1 # move genration on
    CommunityMin <- neutral_generation_speciation(CommunityMin, speciation_rate)#run Neural simulation
    if (Generation <= burn_in_generations ){#if during burn in 
      if (Generation %% interval_rich == 0) {#if an even generation
        RichnessMin[[length(RichnessMin)+1]] <- species_richness(CommunityMin)#store species richness
      }
    } else if (Generation %% interval_oct == 0) {#if after burn in
      AbundanceMin[[length(AbundanceMin)+1]] <-octaves(species_abundance(CommunityMin))#store abundance
    }
  }
  time_taken <- (proc.time() - ptm)[3]
  save(RichnessMin, AbundanceMin,  CommunityMin, time_taken, speciation_rate, size, wall_time,
       interval_rich, interval_oct, burn_in_generations,  file = output_file_name)
  return("Job Done")
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  #call files for the first 25 rds files where size equals 500
  for (i in 1:25) {
    filename <- paste("./my_test_file_",i,".rda")
    load(filename, .GlobalEnv)
    Numer500 <- sum_vect(c(0), AbundanceMin[[1]])
    for (j in 2:length(AbundanceMin)) {
      Numer500 <- sum_vect(Numer500, AbundanceMin[[j]])#work out total abundance across all files
    }
    Dimon500 <- length(AbundanceMin) #work out total length across all files
  }
  #call files for files where size equals 1000
  for (i in 26:51) {
    filename <- paste("./my_test_file_",i,".rda")
    load(filename, .GlobalEnv)
    Numer1000 <- sum_vect(c(0), AbundanceMin[[1]])
    for (j in 2:length(AbundanceMin)) {
      Numer1000 <- sum_vect(Numer1000, AbundanceMin[[j]])
    }
    Dimon1000 <- length(AbundanceMin) 
  } 
  #call files for files where size equals 2500
  for (i in 52:75) {
    filename <- paste("./my_test_file_",i,".rda")
    load(filename, .GlobalEnv)
    Numer2500 <- sum_vect(c(0), AbundanceMin[[1]])
    for (j in 2:length(AbundanceMin)) {
      Numer2500 <- sum_vect(Numer2500, AbundanceMin[[j]])
    }
    Dimon2500 <- length(AbundanceMin) 
  }
  #call files for files where size equals 5000
  for (i in 76:100) {
    filename <- paste("./my_test_file_",i,".rda")
    load(filename, .GlobalEnv)
    Numer5000 <- sum_vect(c(0), AbundanceMin[[1]])
    for (j in 2:length(AbundanceMin)) {
      Numer5000 <- sum_vect(Numer5000, AbundanceMin[[j]])
    }
    Dimon5000 <- length(AbundanceMin) 
  }
  #work out mean abundance for each size
  test500 <- Numer500/Dimon500
  test1000 <- Numer1000/Dimon1000
  test2500 <- Numer2500/Dimon2500
  test5000 <- Numer5000/Dimon5000
  combined_results <- list(test500,test1000,test2500, test5000) #create your list output here to return
  save(combined_results,  file = "process_cluster.rda")# save results to an .rda file
 }

plot_cluster_results <- function()  {
  graphics.off() # clear any existing graphs and plot your graph within the R window
  load("process_cluster.rda", .GlobalEnv) # load combined_results from your rda file
  # put data into dataframes
  Result500 <- data.frame(SppCount500 = combined_results[[1]],
                          frequ = c("1","2-3","4-7","8-15","16-31","32-63","64-127","128-255", "256-511"))
  Result1000 <- data.frame(SppCount1000 = combined_results[[2]],
                           frequ = c("1","2-3","4-7","8-15","16-31","32-63","64-127","128-255", "256-511","512-1023","1024-2047"))
  Result2500 <- data.frame(SppCount2500 = combined_results[[3]],
                           frequ = c("1","2-3","4-7","8-15","16-31","32-63","64-127","128-255", "256-511","512-1023","1024-2047"))
  Result5000 <- data.frame(SppCount5000 = combined_results[[4]],
                          frequ = c("1","2-3","4-7","8-15","16-31","32-63","64-127","128-255", "256-511","512-1023","1024-2047"))
  #plot results
  par(mfrow=c(2,2))
  barplot(Result500$SppCount500, name = Result500$frequ ,main = "Plot of Mean Species Abundance Count \n For Community Size 500 ",
          xlab="Mean Number Individuals per Species Count",
          ylab="Species Count", col=c("red"),
          ylim =c(0, round(max(combined_results[[1]]))))
  barplot(Result1000$SppCount1000, name = Result1000$frequ, main = "Plot of Mean Species Abundance Count \n For Community Size 1000 ",
          xlab="Mean Number Individuals per Species Count",
          ylab="Species Count", col=c("blue"),
          ylim =c(0,max(combined_results[[2]])+1))
  barplot(Result2500$SppCount2500, name = Result2500$frequ, main = "Plot of Mean Species Abundance Count \n For Community Size 2500 ",
          xlab="Mean Number Individuals per Species Count",
          ylab="Species Count", col=c("yellow"),
          ylim =c(0, max(combined_results[[3]])+1))
  barplot(Result5000$SppCount5000, name = Result5000$frequ, main = "Plot of Mean Species Abundance Count \n For Community Size 5000 ",
          xlab="Mean Number Individuals per Species Count",
          ylab="Species Count", col=c("green"),
          ylim =c(0, round(max(combined_results[[4]]))+1))
    return(combined_results)
}

# Question 21
question_21 <- function()  {
    x = log(8)/log(3)
  return(paste("To make the Sierpiński carpet 3 times wider we need 8 times the size. Therefore, when we devide the log of 8 by the log of 3 we find the fractal dimension of the Sierpiński carpet is", x))
}

# Question 22
question_22 <- function()  {
  x = log(20)/log(3)
  return(paste("To make the Menger sponge 3 times wider we need 20 time the size, Therefore, when we devide the log of 20 by the log of 3 we find the fractal dimension of the Menger sponge is", x))
}

# Question 23
chaos_game <- function()  {
  graphics.off() # clear any existing graphs and plot your graph within the R window
  A <- c(0,0)
  B <- c(3,4) 
  C <- c(4,1) 
  D <- list(A,B,C)
  X <- c(0,0)
  
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5) 
  
  for (i in 1:5000) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/2
    X[2] <- (X[2] + E[[1]][2])/2
    points(X[1], X[2], cex=0.1, col =  "blue")
  }
  return("With 100 point we start to see a triangle, with more points the Sierpiński gasket is almost formed though it isn't a true Sierpiński gasket as it isn't equalateral")
}
#smaller point
#not equalateral

# Question 24
turtle <- function(start_position, direction, length)  {
  end_position<- c((cos(direction)*length) + start_position[1], (sin(direction)*length)+start_position[2]) #work out end position based on length and directio
  segments(start_position[1],start_position[2],end_position[1], end_position[2])#draw a line between start and wnd position

  return(end_position) # you should return your endpoint here.
}
# Question 25
elbow <- function(start_position, direction, length)  {
  #initialize start variables 
  x <- start_position
  y <- direction
  z <-  length
  #draw first line and store end position as new_start
  new_start <- turtle(start_position = c(x[1], x[2]),direction = y ,length = z)
  #draw second line based on new start position
  new_line <- turtle(start_position = c(new_start[1], new_start[2]), direction = y - (pi/4), length = (z*0.95))
}

# Question 26
spiral <- function(start_position, direction, length)  {
  #stop resursion
  if (length > 0.2) {
  #initialize start variables
  x <- start_position
  y <- direction
  z <-  length
  #call turtle to make first line
  new_start <- turtle(start_position = c(x[1], x[2]), direction = y, length = z)
  #call spiral to loop and make following lines with new start parameters until recursion stops
  spiral(start_position = c(new_start[1], new_start[2]), direction = y - (pi/4), length = (z*0.95))
  }
  return("Because piral is being called within spiral (recursively) is creates an infinite loop. So internal systems forces a stop to stop the function using all the computational resources of the machine")
 }

# Question 27
draw_spiral <- function()  {
  graphics.off()# clear any existing graphs and plot your graph within the R window
  tempplot <-plot(1, type = "n",# Remove all elements of plot
                  xlab = "", ylab = "",
                  xlim = c(-15, 15), ylim = c(-15, 15))
  spiral(start_position = c(-15, 5), direction =0.785398, length = 10)
}

# Question 28
tree <- function(start_position, direction, length)  {
  #stop recursion
  if (length > 0.2) {
  #initialize start variables
    x <- start_position
    y <- direction
    z <-  length
  # call turtle to draw first line
    new_start <- turtle(c(x[1], x[2]), y , z)
  # call tree making direction go to the right
    tree(c(new_start[1], new_start[2]), y - (pi/4),  (z*0.65))
  # call tree making direction go to the left
    tree(c(new_start[1], new_start[2]), y + (pi/4),  (z*0.65))
  }
}

draw_tree <- function()  {
  graphics.off()# clear any existing graphs and plot your graph within the R window
  tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                  xlab = "", ylab = "",
                  xlim = c(-15, 15), ylim = c(-15, 15)) 
  tree(start_position = c(0, -15), direction =pi/2, length = 10)
}

# Question 29
fern <- function(start_position, direction, length)  {
  
  if (length > 0.2) {
    x <- start_position
    y <- direction
    z <-  length
    new_start <- turtle(c(x[1], x[2]), y , z)
  # call fern to continue direction straight
    fern(c(new_start[1], new_start[2]), y ,  (z*0.87))
  #call fern making direction go to the left
    fern(c(new_start[1], new_start[2]), y + (pi/4),  (z*0.38))
  }
}

draw_fern <- function()  {
  graphics.off()# clear any existing graphs and plot your graph within the R window
  tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                  xlab = "", ylab = "",
                  xlim = c(-15, 15), ylim = c(-15, 15)) 
  fern(start_position = c(0, -15), direction =pi/2, length = 4)
}

# Question 30
fern2 <- function(start_position, direction, length, dir)  {
  if (length > 0.01) {
    x <- start_position
    y <- direction
    z <-  length
    new_start <- turtle(c(x[1], x[2]), y , z)
     dir <- dir * -1 # force switch between 1 and -1 
    fern2(c(new_start[1], new_start[2]), y ,  (z*0.87), dir)
     if (dir == -1) {# if dir = -1 direction goes left
      fern2(c(new_start[1], new_start[2]), y + (pi/4),  (z*0.38), dir * -1)
    }else if (dir == 1) {# if dir = 1 direction goes right
      fern2(c(new_start[1], new_start[2]), y - (pi/4),  (z*0.38), dir)
      
    }
    
  }
}
draw_fern2 <- function()  {
  graphics.off()# clear any existing graphs and plot your graph within the R window
  tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                  xlab = "", ylab = "",
                  xlim = c(-15, 15), ylim = c(-15, 15)) 
  fern2(start_position = c(0, -15), direction =pi/2, length = 4, dir = 1)# dir starts= 1 to force direction to go left first
}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
Challenge_A <- function() {
  graphics.off()# clear any existing graphs and plot your graph within the R window
  CommunityMax<-neutral_generation_speciation(init_community_max(100),0.1)
  CommunityMin<-neutral_generation_speciation(init_community_min(100),0.1)
  #run neutral model with speciation for 200 generations to simulate burn in
  for (i in 1:200) {
    CommunityMax<-neutral_generation_speciation(CommunityMax,0.1)
    CommunityMin<-neutral_generation_speciation(CommunityMin, 0.1)
  }
  #record species richness after burn in
  RichnessMax <- species_richness(CommunityMax)
  RichnessMin <- species_richness(CommunityMin)
  
  #run neutral model with speciation for 2000 generations and record species richnes count
  for (i in 1:2000) {
    CommunityMax<-neutral_generation_speciation(CommunityMax,0.1)
    CommunityMin<-neutral_generation_speciation(CommunityMin,0.1)
      RichnessMax[length(RichnessMax)+1] <-species_richness(CommunityMax)
      RichnessMin[length(RichnessMin)+1] <- species_richness(CommunityMin)
    }
  SumRichnessMax<-sum_vect(RichnessMax[[1]], RichnessMax[[2]])
  SumRichnessMin<-sum_vect(RichnessMin[[1]], RichnessMin[[2]])
  for (i in 3:length(RichnessMax)) {
    SumRichnessMax <- sum_vect(SumRichnessMax, RichnessMax[[i]])
    SumRichnessMin <- sum_vect(SumRichnessMin, RichnessMin[[i]])
  }
}

# Challenge question B
Challenge_B <-  function() {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge question D
Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function() {

  graphics.off() # clear any existing graphs and plot your graph within the R window
  par(mfrow=c(2,2))
  A <- c(0,0)
  B <- c(4,0) 
  C <- c(2,2) 
  D <- list(A,B,C)
  X <- c(4,2)
  
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5) 
  title("Plots of Sierpinski Gasket with \n Different Start Coordinates (X)", line = -19, outer = TRUE)
  for (i in 1:3500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/2
    X[2] <- (X[2] + E[[1]][2])/2
    points(X[1], X[2], cex=0.25, col =  "blue")
  }
  A <- c(0,0)
  B <- c(4,0) 
  C <- c(2,2) 
  D <- list(A,B,C)
  X <- c(2,5)
  
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5) 
  
  for (i in 1:3500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/2
    X[2] <- (X[2] + E[[1]][2])/2
    points(X[1], X[2], cex=0.25, col =  "purple")
  }
  A <- c(0,0)
  B <- c(4,0) 
  C <- c(2,2) 
  D <- list(A,B,C)
  X <- c(0,0)
  
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5) 
  
  for (i in 1:3500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/2
    X[2] <- (X[2] + E[[1]][2])/2
    points(X[1], X[2], cex=0.25, col =  "red")
  }
  A <- c(0,0)
  B <- c(4,0) 
  C <- c(2,2) 
  D <- list(A,B,C)
  X <- c(2,0.75)
  
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5) 
  
  for (i in 1:3500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/2
    X[2] <- (X[2] + E[[1]][2])/2
    points(X[1], X[2], cex=0.25, col =  "gold")
  }
  
  
  #Changing variables
  A <- c(0,0)
  B <- c(2,2) 
  C <- c(4,0) 
  D <- list(A,B,C)
  X <- c(0,0)
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5, main = "Standard plot") 
  for (i in 1:2500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/2
    X[2] <- (X[2] + E[[1]][2])/2
    points(X[1], X[2], cex=0.1, col =  "blue")
  }
  A <- c(0,0)
  B <- c(2,2) 
  C <- c(4,0)
  G <- c(4,1)
  D <- list(A,B,C,G)
  X <- c(0,0)
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5, main = "Extra Coorginate to sample from") 
  for (i in 1:2500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/2
    X[2] <- (X[2] + E[[1]][2])/2
    points(X[1], X[2], cex=0.1, col =  "blue")
  }
  A <- c(0,0)
  B <- c(2,2) 
  C <- c(4,0)
  D <- list(A,B,C)
  X <- c(0,0)
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5, main = "Change distance of movement \n towards next point(/1.5") 
  for (i in 1:2500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/1.5
    X[2] <- (X[2] + E[[1]][2])/1.5
    points(X[1], X[2], cex=0.1, col =  "blue")
  }
  A <- c(0,0)
  B <- c(2,2) 
  C <- c(4,0)
  D <- list(A,B,C)
  X <- c(0,0)
  plot(X[1], X[2], xlim = c(-1,5), ylim = c(-1, 5), cex=0.5, main = "Change distance of movement \n towards next point(/4)" )
  for (i in 1:2500) {
    E <- sample(D, 1)
    X[1] <- (X[1] + E[[1]][1])/4
    X[2] <- (X[2] + E[[1]][2])/4
    points(X[1], X[2], cex=0.1, col =  "blue")
  }
   
  return("Based on coordinate X the points willmove towards and eventually converge to make the Sierpinski Gasket within coordinats A, B and C")
}

# Challenge question F
Challenge_F <- function() {

  Standardfern2 <- function(start_position, direction, length, dir)  {
    if (length > 0.01) {
      x <- start_position
      y <- direction
      z <-  length
      new_start <- turtle(c(x[1], x[2]), y , z)
      dir <- dir * -1 # force switch between 1 and -1 
      Standardfern2(c(new_start[1], new_start[2]), y ,  (z*0.87), dir)
      if (dir == -1) {# if dir = -1 direction goes left
        Standardfern2(c(new_start[1], new_start[2]), y + (pi/4),  (z*0.38), dir * -1)
      }else if (dir == 1) {# if dir = 1 direction goes right
        Standardfern2(c(new_start[1], new_start[2]), y - (pi/4),  (z*0.38), dir)
        
      }
      
    }
  }
  Shortfern2 <- function(start_position, direction, length, dir)  {
    if (length > 0.05) {
      x <- start_position
      y <- direction
      z <-  length
      new_start <- turtle(c(x[1], x[2]), y , z)
      dir <- dir * -1 # force switch between 1 and -1 
       Shortfern2(c(new_start[1], new_start[2]), y ,  (z*0.87), dir)
      if (dir == -1) {# if dir = -1 direction goes left
         Shortfern2(c(new_start[1], new_start[2]), y + (pi/4),  (z*0.38), dir * -1)
      }else if (dir == 1) {# if dir = 1 direction goes right
         Shortfern2(c(new_start[1], new_start[2]), y - (pi/4),  (z*0.38), dir)
        
      }
      
    }
  }
  Longfern2 <- function(start_position, direction, length, dir)  {
    if (length > 0.005) {
      x <- start_position
      y <- direction
      z <-  length
      new_start <- turtle(c(x[1], x[2]), y , z)
      dir <- dir * -1 # force switch between 1 and -1 
      Longfern2(c(new_start[1], new_start[2]), y ,  (z*0.87), dir)
      if (dir == -1) {# if dir = -1 direction goes left
        Longfern2(c(new_start[1], new_start[2]), y + (pi/4),  (z*0.38), dir * -1)
      }else if (dir == 1) {# if dir = 1 direction goes right
        Longfern2(c(new_start[1], new_start[2]), y - (pi/4),  (z*0.38), dir)
        
      }
      
    }
  }
  
  Challengeturtle <- function(start_position, direction, length, Colour)  {
    end_position<- c((cos(direction)*length) + start_position[1], (sin(direction)*length)+start_position[2]) #work out end position based on length and directio
    segments(start_position[1],start_position[2],end_position[1], end_position[2],
             col = Colour)#draw a line between start and wnd position
    
    return(end_position) # you should return your endpoint here.
  }
  Xmassfern2 <- function(start_position, direction, length, dir, Colour)  {
    if (length > 0.01) {
      x <- start_position
      y <- direction
      z <-  length
      Col <- Colour
      new_start <- Challengeturtle(c(x[1], x[2]), y , z, Col)
      dir <- dir * -1 # force switch between 1 and -1 
      Xmassfern2 (c(new_start[1], new_start[2]), y ,  (z*0.87), dir, Colour=c("dark green"))
      if (dir == -1) {# if dir = -1 direction goes left
        Xmassfern2 (c(new_start[1], new_start[2]), y + (pi/4),  (z*0.38), dir * -1, Colour=c("red"))
      }else if (dir == 1) {# if dir = 1 direction goes right
        Xmassfern2 (c(new_start[1], new_start[2]), y - (pi/4),  (z*0.38), dir, Colour = c("dark green"))
        
      }
      
    }
  }

    graphics.off()# clear any existing graphs and plot your graph within the R window
    par(mfrow=c(2,2))
     tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                    xlab = "", ylab = "",
                    xlim = c(-15, 15), ylim = c(-15, 15),
                    main ="Standard fern line size threshold 0.01") 
    
    Standardfern2(start_position = c(0, -15), direction =pi/2, length = 4, dir = 1)# dir starts= 1 to force direction to go left first
    
    tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                    xlab = "", ylab = "",
                    xlim = c(-15, 15), ylim = c(-15, 15),
                    main ="Short fern line size threshold 0.05")
    
    Shortfern2(start_position = c(0, -15), direction =pi/2, length = 4, dir = 1)
    
    tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                    xlab = "", ylab = "",
                    xlim = c(-15, 15), ylim = c(-15, 15),
                    main ="Long fern line size threshold 0.005")
    
    Longfern2(start_position = c(0, -15), direction =pi/2, length = 4, dir = 1)
    tempplot <-plot(1, type = "n",                         # Remove all elements of plot
                    xlab = "", ylab = "",
                    xlim = c(-15, 15), ylim = c(-15, 15),
                    main ="Happy Christmas!")
    
    Xmassfern2(start_position = c(0, -15), direction =pi/2, length = 4, dir = 1, Colour = "brown")
    
    
  return("The smaller the line size threshold the longer the function takes to run and the more dence the brancges are.")
}

# Challenge question G should be written in a separate file that has no dependencies on any functions here.


