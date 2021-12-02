

## results



Compare_AICc<-data.frame(Subset =  1:max(DF$ID),
                          Poly_AIC = Stat_Poly["Poly_AICc"],
                          Gromp_AIC = Stat_Gromp["Gromp_AICc"])
                         # Baran_AIC = AIC_Baran["Baran_AIC"])

Compare_BIC<-data.frame(Subset =  1:max(DF$ID),
                         Poly_BIC = Stat_Poly["Poly_BIC"],
                         Gromp_BIC = Stat_Gromp["Gromp_BIC"])
# Baran_AIC = AIC_Baran["Baran_AIC"])


# for 1 in 1:max ID
# which bigger compare model AIC
# into best model model name and AIC

Best_model<-data.frame(Subset = 1:max(DF$ID),
               ModelName = "NA",
               AICc ="NA",
               BIC="NA")
for (i in 1:max(DF$ID)) {
  
  if (Compare_AICc[i, "Poly_AICc"] < Compare_AICc[i, "Gromp_AICc"]){# && Compare_model[i, "Poly_AICc"]) < Compare_model[i, "Baran_AICc"]){
    Best_model[i,"AICc"]<- Compare_AICc[i, "Poly_AICc"]
    Best_model[i,"ModelName"] <- "Cubic Poly"
  }
  else if (Compare_AICc[i, "Gromp_AICc"] < Compare_AICc[i, "Poly_AICc"]){# && Compare_model[i, "Gromp_AIC"] < Compare_model[i, "Baran_AIC"]){
    Best_model[i,"AICc"]<- Compare_AICc[i, "Gromp_AICc"]
    Best_model[i,"ModelName"] <- "Grompertz"
  }
  #else if (Compare_model[i, "Baran_AIC"] < Compare_model[i, "Poly_AIC"] && Compare_model[i, "Baran_AIC"] < Compare_model[i, "Gromp_AIC"]){
   # h[i,"AIC"]<- Compare_model[i, "Baran_AIC"]
    #h[i,"ModelName"] <- "Baranyi"
  #}
  if (Compare_BIC[i, "Poly_BIC"] < Compare_BIC[i, "Gromp_BIC"]){# && Compare_model[i, "Poly_BIC"]) < Compare_model[i, "Baran_BIC"]){
    Best_model[i,"BIC"]<- Compare_BIC[i, "Poly_BIC"]
    Best_model[i,"ModelName"] <- "Cubic Poly"
  }
  else if (Compare_BIC[i, "Gromp_BIC"] < Compare_BIC[i, "Poly_BIC"]){# && Compare_model[i, "Gromp_AIC"] < Compare_model[i, "Baran_AIC"]){
    Best_model[i,"BIC"]<- Compare_BIC[i, "Gromp_BIC"]
    Best_model[i,"ModelName"] <- "Grompertz"
  }
}

###
