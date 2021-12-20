
rm(list=ls())

## Imports ##
require(viridis)
require(dplyr)
require(ggplot2)

DF <- read.csv("../data/ModGrowthData.csv")
Best_model<-read.csv("../result/BestModel.csv")

# table of best model #
h <- Best_model %>% count(BestModelAIC)
f <- Best_model %>% count(BestModelAICc)
g <- Best_model %>% count(BestModelBIC)
BestAIC <- data.frame(Models = h[,1],
                       Percent = c(((h[1,2]/277)*100),((h[2,2]/277)*100),((h[3,2]/277)*100)))
write.csv(BestAIC, "../result/BestAICModel.csv")
AICcMod <- which(f[,1] != "NA")
BestAICc <- data.frame(Models = f[AICcMod,1],
                      Percent = c(((f[1,2]/277)*100),((f[2,2]/277)*100),((f[3,2]/277)*100)))
write.csv(BestAICc, "../result/BestAICcModel.csv")
BestBIC <- data.frame(Models = g[,1],
                      Percent = c(((g[1,2]/277)*100),((g[2,2]/277)*100),((g[3,2]/277)*100)))
write.csv(BestBIC, "../result/BestBICModel.csv")


# Graph for Temp data #
k <- Best_model[which(Best_model[5] != "NA"),]
TempData <- data.frame(Subset = 1:max(DF$ID),
                     Temp = 0,
                     Bestmodel = Best_model$BestModelAICc,
                     stringsAsFactors = F)
TempData <- data.frame(Subset = k$Subset,
                      Temp = 0,
                      Bestmodel = k$BestModelAICc,
                      stringsAsFactors = F)

for (i in 1:length(k$Subset)) {
  
  d <- DF[ which(DF$ID == k$Subset[i]),]
  TempData[i,"Temp"] <- d$Temp[1]
  
}



TempPlot <- ggplot(data = TempData) +
  aes(x = Bestmodel, fill = as.factor(Temp))+
  geom_bar()+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  xlab('Model')+
  ylab('Count')



ggsave(TempPlot, file= paste0("../result/TempBarGraph.pdf"))
