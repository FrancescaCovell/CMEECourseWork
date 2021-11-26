

## results



Compare_model<-data.frame(Subset =  1:max(DF$ID),
                          Poly_AIC = AIC_Poly["Poly_AIC"],
                          Gromp_AIC = AIC_Gromp["Gromp_AIC"],
                          Baran_AIC = AIC_Baran["Baran_AIC"])



# for 1 in 1:max ID
# which bigger compare model AIC
# into best model model name and AIC

h <-data.frame(Subset = 1:max(DF$ID),
               ModelName = "NA",
               AIC ="NA")
for (i in 1:max(DF$ID)) {
  
  if (Compare_model[i, "Poly_AIC"] < Compare_model[i, "Gromp_AIC"] && Compare_model[i, "Poly_AIC"] < Compare_model[i, "Baran_AIC"]){
    h[i,"AIC"]<- Compare_model[i, "Poly_AIC"]
    h[i,"ModelName"] <- "Cubic Poly"
  }
  else if (Compare_model[i, "Gromp_AIC"] < Compare_model[i, "Poly_AIC"] && Compare_model[i, "Gromp_AIC"] < Compare_model[i, "Baran_AIC"]){
    h[i,"AIC"]<- Compare_model[i, "Gromp_AIC"]
    h[i,"ModelName"] <- "Grompertz"
  }
  else if (Compare_model[i, "Baran_AIC"] < Compare_model[i, "Poly_AIC"] && Compare_model[i, "Baran_AIC"] < Compare_model[i, "Gromp_AIC"]){
    h[i,"AIC"]<- Compare_model[i, "Baran_AIC"]
    h[i,"ModelName"] <- "Baranyi"
  }
        }
###
Best_model<-data.frame(Subset = 1:max(DF$ID),
                     ModelName = "NA",
                     AIC ="NA",
                     BIC = "NA",
                     Rsqr = "NA")