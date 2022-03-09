
library(tidyverse)
library(readr)
library(gtools)
library(viridis)
library(parallel)

#gpath = "/home/ben/Documents/PhD/SLOSS_TNM/Results/Legacy_Outputs/no_Immigration_Radius"
gpath = "/home/frcovell/Project/Results/TNM_Output"
gpath = "/home/frcovell/Project/sandbox/Testing intraspecific competition/Intra-0.1"
gpath = "/home/frcovell/Project/sandbox/Par test results/Disp and interact change/Dist1Inter1"
setwd(gpath)

random_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "RandomLandscape1_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()
random_Paths = append(random_Paths, list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "/Landscape1") %>% str_subset(pattern = "txt/Results") %>% mixedsort())

## Function to read in any TNM output from any seed, giving the path and the data you want as input
read_Output = function(res_Path, res) {
  
  if(res == "/totalPop.txt") {
    cols = c("g", "n")
  } else if(res == "/totalPopSpec.txt") {
    cols = c("g", "s", "n")
  } else if(res == "/cellPop.txt") {
    cols = c("g", "c", "n")
  } else if(res == "/cellPopSpec.txt") {
    cols = c("g", "c", "s", "n")
  } else if(res == "/cellRich.txt") {
    cols = c("g", "c", "n")
  }
  
  if(res == "/cellPopSpec.txt") {
    f <- function(x, pos) subset(x, g == 9976)
    data = read_delim_chunked(file = paste0(res_Path, res), delim = " ", col_names = cols, chunk_size = 10000, callback = DataFrameCallback$new(f), 
                              progress = FALSE)
  } else {
    data = read.table(file = paste0(res_Path, res), col.names = cols)
  }
  
  seed = str_split(res_Path, pattern = "/")[[1]][10] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
  run = str_split(res_Path, pattern = "/")[[1]][8]%>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
  patch = str_split(res_Path, pattern = "/")[[1]][11] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
  
  data = data %>% add_column(seed = seed, run = run, patch = patch)
  
  return(data)
  
}

totalPops = mcmapply(read_Output, random_Paths, "/totalPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
totalPopSpecs = mcmapply(read_Output, random_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPops = mcmapply(read_Output, random_Paths, "/cellPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellRich = mcmapply(read_Output, random_Paths, "/cellRich.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPopSpec = mcmapply(read_Output, random_Paths, "/cellPopSpec.txt", SIMPLIFY = F, mc.cores = 10) %>% bind_rows()

## Function to calculate standard error
SE = function(x) {
  sample_Size = length(x)
  SD = sd(x)
  SE = SD/sqrt(sample_Size)
  return(SE)
}

######################################
## Total Population average for patches, averaging across seed and runs
######################################

totalPops_SE = totalPops %>% group_by(g, patch) %>% summarise(mean_n = mean(n), SE = SE(n))
ggplot(totalPops_SE, aes(x = g, y = mean_n, fill = patch)) + 
  geom_line() +
  geom_ribbon(aes(ymax = mean_n + SE, ymin = mean_n - SE), alpha = 0.2) + 
  labs(title ="Amalagamation intraspecific competition -0.1", x = "Generation", y = "Mean Total Population") + 
  theme_bw()


richness_SE = totalPopSpecs %>% filter(n > 0) %>% group_by(g, seed, run, patch) %>% summarise(rich = length(n)) %>% ungroup() %>% 
  group_by(patch, g) %>% summarise(mean_Rich = mean(rich), SE = SE(rich))

ggplot(richness_SE, aes(x = g, y = mean_Rich, fill = patch)) + geom_line() + 
  geom_ribbon(aes(ymax = mean_Rich + SE, ymin = mean_Rich - SE), alpha = 0.2) +
  labs(title ="Amalagamation intraspecific competition -0.1", x = "Generation", y = "Mean Species Richness") + 
  theme_bw()
