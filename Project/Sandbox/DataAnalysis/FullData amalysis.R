library(tidyverse)
library(readr)
library(gtools)
library(viridis)
library(parallel)
library(ggplot2)
library(gridExtra)

#gpath = "/home/frcovell/Project/Results/TNM_Output"
gpath = "/home/frcovell/Project/sandbox/Par test results/Testing intraspecific competition/Intra0"
setwd(gpath)

# function 
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
  
  seed = str_split(res_Path, pattern = "/")[[1]][8] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
  run = str_split(res_Path, pattern = "/")[[1]][10] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
  patch = str_split(res_Path, pattern = "/")[[1]][11] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)%>% str_split(., pattern = ".t") %>% simplify() %>% nth(1)
  
  data = data %>% add_column(seed = seed, run = run, patch = patch)
  
  return(data)
  
}

SE = function(x) {
  sample_Size = length(x)
  SD = sd(x)
  SE = SD/sqrt(sample_Size)
  return(SE)
}

#### Amalgamtion ####

amalgamation_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "AmalgamationLandscape1_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()
#amalgamation_Paths = append(amalgamation_Paths, list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "/Landscape1") %>% str_subset(pattern = "txt/Results") %>% mixedsort())

AmatotalPops = mcmapply(read_Output, amalgamation_Paths, "/totalPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
AmatotalPopSpecs = mcmapply(read_Output, amalgamation_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
AmacellPops = mcmapply(read_Output, amalgamation_Paths, "/cellPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
AmacellRich = mcmapply(read_Output, amalgamation_Paths, "/cellRich.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
AmacellPopSpec = mcmapply(read_Output, amalgamation_Paths, "/cellPopSpec.txt", SIMPLIFY = F, mc.cores = 10) %>% bind_rows()

AmatotalPops_SE = AmatotalPops %>% group_by(g, patch) %>% summarise(mean_n = mean(n), SE = SE(n))
Amarichness_SE = AmatotalPopSpecs %>% filter(n > 0) %>% group_by(g, seed, run, patch) %>% summarise(rich = length(n)) %>% ungroup() %>% 
  group_by(patch, g) %>% summarise(mean_Rich = mean(rich), SE = SE(rich))

#### Random ####

random_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "RandomLandscape1_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()
#random_Paths = append(random_Paths, list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "/Landscape1") %>% str_subset(pattern = "txt/Results") %>% mixedsort())

RandtotalPops = mcmapply(read_Output, random_Paths, "/totalPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RandtotalPopSpecs = mcmapply(read_Output, random_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RandcellPops = mcmapply(read_Output, random_Paths, "/cellPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RandcellRich = mcmapply(read_Output, random_Paths, "/cellRich.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RandcellPopSpec = mcmapply(read_Output, random_Paths, "/cellPopSpec.txt", SIMPLIFY = F, mc.cores = 10) %>% bind_rows()

RandtotalPops_SE = RandtotalPops %>% group_by(g, patch) %>% summarise(mean_n = mean(n), SE = SE(n))
Randrichness_SE = RandtotalPopSpecs %>% filter(n > 0) %>% group_by(g, seed, run, patch) %>% summarise(rich = length(n)) %>% ungroup() %>% 
  group_by(patch, g) %>% summarise(mean_Rich = mean(rich), SE = SE(rich))
#### Raster ####

raster_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "RasterLandscape") %>% str_subset(pattern = "txt/Results") %>% mixedsort()
#raster_Paths = append(raster_Paths, list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "/Landscape") %>% str_subset(pattern = "txt/Results") %>% mixedsort())


RasttotalPops = mcmapply(read_Output, raster_Paths, "/totalPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RasttotalPopSpecs = mcmapply(read_Output, raster_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RastcellPops = mcmapply(read_Output, raster_Paths, "/cellPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RastcellRich = mcmapply(read_Output, raster_Paths, "/cellRich.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
RastcellPopSpec = mcmapply(read_Output, raster_Paths, "/cellPopSpec.txt", SIMPLIFY = F, mc.cores = 10) %>% bind_rows()

RasttotalPops_SE = RasttotalPops %>% group_by(g, patch) %>% summarise(mean_n = mean(n), SE = SE(n))
Rastrichness_SE = RasttotalPopSpecs %>% filter(n > 0) %>% group_by(g, seed, run, patch) %>% summarise(rich = length(n)) %>% ungroup() %>% 
  group_by(patch, g) %>% summarise(mean_Rich = mean(rich), SE = SE(rich))

#### satistic ####

#effect of pecentage cover
#total pop
AmaTtlPopData <- as.data.frame(AmatotalPops_SE)
AmaAno<- summary(aov(mean_n ~ patch, AmaTtlPopData))
AmaLm<-summary(lm(mean_n ~ patch, AmaTtlPopData))


RandTtlPopData <- as.data.frame(RandtotalPops_SE)
RandAno<- summary(aov(mean_n ~ patch, RandTtlPopData))
RandLm<-summary(lm(mean_n ~ patch, RandTtlPopData))

RastTtlPopData <- as.data.frame(RasttotalPops_SE)
RastAno<- summary(aov(mean_n ~ patch, RastTtlPopData))
RastLm<-summary(lm(mean_n ~ patch, RastTtlPopData))

TtlPopAma <- ggplot(AmaTtlPopData, aes(x=patch, y=mean_n)) + 
  labs(title="Amalgamation Generated Landscape",x="Percentage Cover", y = "Mean Total population")+
  geom_boxplot(outlier.shape=NA)
TtlPopRand <- ggplot(RandTtlPopData, aes(x=patch, y=mean_n)) + 
  labs(title="Randomly Generate Landscape",x="Percentage Cover", y = "Mean Total population")+
  geom_boxplot(outlier.shape=NA)
TtlPopRast <- ggplot(RastTtlPopData, aes(x=patch, y=mean_n)) + 
  labs(title="Raster Generated Landscape",x="Percentage Cover", y = "Mean Total population")+
  geom_boxplot(outlier.shape=NA)

grid.arrange(TtlPopAma,TtlPopRand,TtlPopRast, nrow=1,  top = "Plot of Mean Total per Percentage Cover")


#Species Richness 
AmaSppRchData <- as.data.frame(Amarichness_SE)
AmaAno<- summary(aov(mean_Rich ~ patch, AmaSppRchData))
AmaLm<-summary(lm(mean_Rich ~ patch, AmaSppRchData))

RandSppRchData <- as.data.frame(Randrichness_SE)
RandAno<- summary(aov(mean_Rich ~ patch, RandSppRchData))
RandLm<-summary(lm(mean_Rich ~ patch, RandSppRchData))

RastSppRchData <- as.data.frame(Rastrichness_SE)
RastAno<- summary(aov(mean_Rich ~ patch, RastSppRchData))
RastLm<-summary(lm(mean_Rich ~ patch, RastSppRchData))

SppRchAma <- ggplot(AmaSppRchData, aes(x=patch, y=mean_Rich)) + 
  labs(title="Amalgamation Generated Landscape",x="Percentage Cover", y = "Mean Total population")+
  geom_boxplot(outlier.shape=NA)
SppRchRand <- ggplot(RandSppRchData, aes(x=patch, y=mean_Rich)) + 
  labs(title="Random Generated Landscape",x="Percentage Cover", y = "Mean Total population")+
  geom_boxplot(outlier.shape=NA)
SppRchRast <- ggplot(RastSppRchData, aes(x=patch, y=mean_Rich)) + 
  labs(title="Raster Generated Landscape",x="Percentage Cover", y = "Mean Total population")+
  geom_boxplot(outlier.shape=NA)

grid.arrange(SppRchAma,SppRchRand,SppRchRast, ncol=3,  top = "Plot of Species Richness per Percentage Cover")




FullTtlpopData <- data.frame(LandGen = c(rep("Amalgamation", length(AmatotalPops_SE$patch)), rep("Random", length(RandtotalPops_SE$patch)), rep("Raster", length(RasttotalPops_SE$patch))),
                       Patch = c(AmatotalPops_SE$patch, RandtotalPops_SE$patch, RasttotalPops_SE$patch),
                       Mean_n = c(AmatotalPops_SE$mean_n,RandtotalPops_SE$mean_n, RasttotalPops_SE$mean_n))
FullRichData <- data.frame(LandGen = c(rep("Amalgamation", length(Amarichness_SE$patch)), rep("Random", length(Randrichness_SE$patch)), rep("Raster", length(Rastrichness_SE$patch))),
                             Patch = c(Amarichness_SE$patch, Randrichness_SE$patch, Rastrichness_SE$patch),
                             Mean_Rich = c(Amarichness_SE$mean_Rich,Randrichness_SE$mean_Rich, Rastrichness_SE$mean_Rich))

#total pop
#percentage cover
PatLm <- summary(lm(Mean_n ~ Patch, FullTtlpopData))
#effect of landscape generation
LanLm <- summary(lm(Mean_n ~ LandGen, FullTtlpopData))

FullLm<-summary(lm(Mean_n ~ Patch+LandGen, FullTtlpopData))

#species richness
#percentage cover
PatLm <- summary(lm(Mean_Rich ~ Patch, FullRichData))
#effect of landscape generation
LanLm <- summary(lm(Mean_Rich~ LandGen, FullRichData))

FullLm<-summary(lm(Mean_Rich ~ Patch+LandGen, FullRichData))

ggplot(FullTtlpopData, aes(x=Patch, y=Mean_n)) + 
  labs(title="Plot of Mean Total per Percentage Cover",x="Percentage Cover", y = "Mean Total population")+
  geom_boxplot(outlier.shape=NA)

ggplot(FullRichData, aes(x=Patch, y=Mean_Rich)) + 
  labs(title="Plot of Species Richness per Percentage Cover",x="Percentage Cover", y = "Species Richness")+
  geom_boxplot(outlier.shape=NA)
