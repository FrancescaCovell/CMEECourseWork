require(viridis)
LetsGO <- data.frame(Subset = 1:max(DF$ID),
                     Temp = 0,
                     Bestmodel = Best_model$BestModelAIC,
                     stringsAsFactors = F)

for (i in 1:max(DF$ID)) {
  d <- DF[ which(DF$ID == i),]
  LetsGO[i,"Temp"] <- d$Temp[1]
  
}
LetsGO["Temp"]

TempPlot <- ggplot(data = LetsGO) +
  aes(x = Bestmodel, fill = Temp)+
  geom_bar()+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  xlab('Model')+
  ylab('Count')
