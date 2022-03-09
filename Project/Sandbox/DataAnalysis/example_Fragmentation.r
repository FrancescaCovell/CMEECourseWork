##############################################################
## Looking at different fragment seed runs of the same
## base community from the new TNM
##
## Ben Howes, 2nd Year PhD Student, Imperial College London
##############################################################

library(tidyverse)
library(readr)
library(gtools)
library(viridis)
library(parallel)

#gpath = "/home/ben/Documents/PhD/SLOSS_TNM/Results/Legacy_Outputs/no_Immigration_Radius"
gpath = "/home/frcovell/Project/Results/TNM_Output/Seed_1/"
setwd(gpath)

#### Read in paths for results for all seeds and runs
raster_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "landscape_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()
random_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "RandomLandscape1_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()
amalgamation_Paths = list.dirs(path = gpath, recursive = TRUE) %>% str_subset(pattern = "AmalgamationLandscape1_") %>% str_subset(pattern = "txt/Results") %>% mixedsort()

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
    run = str_split(res_Path, pattern = "/")[[1]][7] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)
    patch = str_split(res_Path, pattern = "/")[[1]][11] %>% str_split(., pattern = "_") %>% simplify() %>% nth(2)

    data = data %>% add_column(seed = seed, run = run, patch = patch)

    return(data)

}

## Load in all data we are interested in and bind
totalPops = mcmapply(read_Output, patch_Paths, "/totalPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
totalPopSpecs = mcmapply(read_Output, patch_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPops = mcmapply(read_Output, patch_Paths, "/cellPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellRich = mcmapply(read_Output, patch_Paths, "/cellRich.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPopSpec = mcmapply(read_Output, patch_Paths, "/cellPopSpec.txt", SIMPLIFY = F, mc.cores = 10) %>% bind_rows()


totalPops = mcmapply(read_Output, random_Paths, "/totalPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
totalPopSpecs = mcmapply(read_Output, random_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPops = mcmapply(read_Output, random_Paths, "/cellPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellRich = mcmapply(read_Output, random_Paths, "/cellRich.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPopSpec = mcmapply(read_Output, random_Paths, "/cellPopSpec.txt", SIMPLIFY = F, mc.cores = 10) %>% bind_rows()


totalPops = mcmapply(read_Output, patch_Paths, "/totalPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
totalPopSpecs = mcmapply(read_Output, patch_Paths, "/totalPopSpec.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPops = mcmapply(read_Output, patch_Paths, "/cellPop.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellRich = mcmapply(read_Output, patch_Paths, "/cellRich.txt", SIMPLIFY = F, mc.cores = 6) %>% bind_rows()
cellPopSpec = mcmapply(read_Output, patch_Paths, "/cellPopSpec.txt", SIMPLIFY = F, mc.cores = 10) %>% bind_rows()

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

totalPops_SE = totalPops %>% group_by(g, patch) %>% summarise(mean_n = mean(n), SE = SE(totalPops$n)) 
ggplot(totalPops_SE, aes(x = g, y = mean_n, fill = patch)) + 
  geom_line() +
  geom_ribbon(aes(ymax = mean_n + SE, ymin = mean_n - SE), alpha = 0.2) + 
  labs(x = "Generation", y = "Mean Total Population") + 
  theme_bw()

######################################
## Species Richness
######################################

richness_SE = totalPopSpecs %>% filter(n > 0) %>% group_by(g, seed, run, patch) %>% summarise(rich = length(n)) %>% ungroup() %>% 
    group_by(patch, g) %>% summarise(mean_Rich = mean(rich), SE = SE(rich))

ggplot(richness_SE, aes(x = g, y = mean_Rich, fill = patch)) + geom_line() + 
    geom_ribbon(aes(ymax = mean_Rich + SE, ymin = mean_Rich - SE), alpha = 0.2) +
        labs(x = "Generation", y = "Mean Species Richness") + 
            theme_bw()

########################################
## Population change in matrix cells
########################################

matrix_1F = read.table(paste0(gpath, "/Seed_1/Fragmentation/Seed_1/landscape_1LF.txt/cellList.txt")) %>% mutate(V1 = V1+1) %>% rename(cell = V1, type = V2) %>% add_column(patch = "1LF.txt")
matrix_3F = read.table(paste0(gpath, "/Seed_1/Fragmentation/Seed_1/landscape_3F.txt/cellList.txt")) %>% mutate(V1 = V1+1) %>% rename(cell = V1, type = V2) %>% add_column(patch = "3F.txt")
matrix_9F = read.table(paste0(gpath, "/Seed_1/Fragmentation/Seed_1/landscape_9F.txt/cellList.txt")) %>% mutate(V1 = V1+1) %>% rename(cell = V1, type = V2) %>% add_column(patch = "9F.txt")

matrix_Cells = rbind(matrix_1F, matrix_3F, matrix_9F)

cellPops = cellPops %>% left_join(matrix_Cells, by = c("patch" = "patch", "c" = "cell"))

sum_CellPops = cellPops %>% group_by(g, seed, run, patch, type) %>% summarise(pop = sum(n))

cellPops_SE = sum_CellPops %>% group_by(g, patch, type) %>% summarise(mean_n = mean(pop), SE = SE(pop))

ggplot(cellPops_SE, aes(x = g, y = mean_n, fill = patch)) + geom_line(aes(col = patch)) + 
    geom_ribbon(aes(ymax = mean_n + SE, ymin = mean_n - SE), alpha = 0.2) +
        facet_wrap(. ~ type, labeller = as_labeller(c('0' = "Matrix", '1' = "Forest"))) + 
            labs(x = "Generations", y = "Mean Total Population") + theme_bw()


cell_XY = data.frame(cell = seq(1, 121, by = 1), x = rep(seq(1, 11, by = 1), times = 11), y = rep(seq(11, 1, by = -1), each = 11))

cellPops_XY = cellPops %>% left_join(cell_XY, by = c("c" = "cell")) %>% group_by(g, c, patch, x, y) %>% summarise(median_n = median(n))

dat = filter(cellPops_XY, g == 9976)

ggplot(dat, aes(x = x, y = y, fill = median_n)) + geom_raster() + coord_equal() +
    geom_text(data = dat, label = dat$median_n) + facet_wrap(. ~ patch) +
        scale_fill_viridis() + theme_bw() + theme(legend.position = "none") +
            ggtitle("Median population per cell")

########################################
## Cell Richness
########################################

cellRich = cellRich %>% left_join(matrix_Cells, by = c("patch" = "patch", "c" = "cell"))

dellRich_XY = cellRich %>% left_join(cell_XY, by = c("c" = "cell")) %>% group_by(g, c, patch, x, y) %>% summarise(median_rich = median(n))

dat1 = filter(cellRich_XY, g == 9976)

ggplot(dat1, aes(x = x, y = y, fill = median_rich)) + geom_raster() + coord_equal() +
    geom_text(data = dat1, label = dat1$median_rich) + facet_wrap(. ~ patch) +
        scale_fill_viridis() + theme_bw() + theme(legend.position = "none") +
            ggtitle("Median richness per cell")

########################################
## Cell Species Composition
########################################

cellPopSpec_Avg_Seed = cellPopSpec %>% group_by(c, s, seed, patch) %>% summarise(median_n = median(n)) %>% group_by(seed, patch)

## Find which species have a population above 0 in the community
pres_Spec = cellPopSpec_Avg_Seed %>% group_by(s, seed, patch) %>% summarise(pop = sum(median_n)) %>% filter(pop > 0)

## Filter to only keep species which have a population above 0 in the community
cellPopSpec_Avg_Seed = cellPopSpec_Avg_Seed %>% left_join(pres_Spec) %>% filter(!is.na(pop))

## Calculate percentage of cell population taken up by each species
cellPopSpec_Avg_Seed = cellPopSpec_Avg_Seed %>% group_by(c, seed, patch) %>% mutate(perc_Pop = (median_n/sum(median_n)*100))
cellPopSpec_Avg_Seed[is.na(cellPopSpec_Avg_Seed$perc_Pop),]$perc_Pop = 0

## Bind with data on which cells are matrix/forest
cellPopSpec_Avg_Seed = cellPopSpec_Avg_Seed %>% left_join(matrix_Cells, by = c("c" = "cell", "patch" = "patch"))

test = cellPopSpec_Avg_Seed %>% filter(seed == 10)

test1 = left_join(test, cell_XY, by = c("c" = "cell"))

ggplot(test1, aes(x = x, y = y, fill = perc_Pop)) + geom_tile() + 
    coord_equal() + geom_text(data = test1, label = round(test1$median_n,1), size = 3) + facet_grid(s ~ patch) +
            scale_fill_viridis() + theme_bw() + ggtitle("Numbers = Population, Colour = Percentage of Cell Population")
