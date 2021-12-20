## Modeling data

# Required
require(minpack.lm)
require(ggplot2)
require(MuMIn)
require(tictoc)

# Import data
rm(list=ls())

DF <- read.csv("../data/ModGrowthData.csv")

# Models
 gompertz_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
   return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
 }   

 
 baranyi_model <- function(t, r_max, K, N_0, t_lag){ 
   A <- t + 1/ r_max * log(exp(-r_max * t) + exp(-t_lag) - exp(-r_max * t - t_lag))
   return(N_0 + r_max * A - log(1+(exp(r_max * A)-1/ exp(K - N_0 ))))
 }  
 

# Initialised Stats output
Stat_Poly <-data.frame(Subset = 1:max(DF$ID),
                       Poly_AIC = 0,
                       Poly_AICc = 0,
                       Poly_BIC = 0)
Stat_Gromp <-data.frame(Subset = 1:max(DF$ID),
                       Gromp_AIC = 0,
                       Gromp_AICc = 0,
                       Gromp_BIC = 0)
Stat_Baran <-data.frame(Subset = 1:max(DF$ID),
                        Baran_AIC = 0,
                        Baran_AICc = 0,
                        Baran_BIC = 0)


tic() 
for (i in 1:max(DF$ID)) {
  tryCatch(
    expr = {
      test <- DF[ which(DF$ID == i),]
      
      # Cubic Polynomial #
      Poly <- lm(test$log_PopBio ~ poly(test$Time,3, raw = T))
      
      Stat_Poly[i,"Poly_AICc"]<-AICc(Poly)
      Stat_Poly[i,"Poly_BIC"]<- BIC(Poly)
      Stat_Poly[i,"Poly_AIC"]<-AIC(Poly)
      
      
      Lag <- test[1:round(length(test$Time)/2),]
      Station <-  test[(round(length(test$Time)/2)+1):length(test$Time),]
      
      lm_growth <- lm(log_PopBio  ~ Time, data = subset(DF, ID == i & Lag$Time[which.max(diff(diff(log(Lag$PopBio))))] & Time <  Station$Time[which.max(diff(diff(log(Station$PopBio))))]))
      coef(lm_growth)["Time"]# use to find r_man_start
      r_max_start <- coef(lm_growth)["Time"] # use our previous estimate from the OLS fitting from above
      t_lag_start <- Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]# find last timepoint of lag phase
      
      test_Gromp <- data.frame(matrix( ncol = 3, nrow = 100))
      test_Baran <- data.frame(matrix( ncol = 3, nrow = 100))
      x <- c("AIC", "AICc", "BIC")
      colnames(test_Gromp) <- x
      colnames(test_Baran) <- x
      
      for (j in 1:100) {
        tryCatch(
          expr = {
            N_0_start <-rnorm(1,min(test$log_PopBio ))#, sd = 3 * min(test$log_PopBio )) # lowest population size, note log scale
            K_start <- rnorm(1,max(test$log_PopBio ))#, sd = 3*2*max(test$log_PopBio )) # highest population size, note log scale
            fit_gompertz <- nlsLM(log_PopBio  ~ gompertz_model(t = Time, r_max, K, N_0, t_lag), test,
                                  list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100),trace = T)
            fit_baranyi <- nlsLM(log_PopBio  ~ baranyi_model(t = Time, r_max, K, N_0, t_lag), test,
                                 list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100),trace = T)
            
            test_Gromp[j,"AIC"] <- AIC(fit_gompertz)
            test_Gromp[j,"AICc"] <- AICc(fit_gompertz)
            test_Gromp[j,"BIC"] <- BIC(fit_gompertz)
            test_Baran[j,"AIC"] <- AIC(fit_baranyi)
            test_Baran[j,"AICc"] <- AICc(fit_baranyi)
            test_Baran[j,"BIC"] <- BIC(fit_baranyi)
            
          },
          error = function(e){
            message('Caught an error!')
            print(e)
          },
          warning = function(w){
            message('Caught an warning!')
            print(w)
          },
          finally = {
            message('All done, quitting.')
          })
      }
      Stat_Gromp[i,"Gromp_AICc"]<-min(test_Gromp$AICc[!is.na(test_Gromp$AICc)])
      Stat_Gromp[i,"Gromp_BIC"]<- min(test_Gromp$BIC[!is.na(test_Gromp$BIC)])
      Stat_Gromp[i,"Gromp_AIC"]<-min(test_Gromp$AIC[!is.na(test_Gromp$AIC)])
      Stat_Baran[i,"Baran_AICc"]<-min(test_Baran$AICc[!is.na(test_Baran$AICc)])
      Stat_Baran[i,"Baran_BIC"]<- min(test_Baran$BIC[!is.na(test_Baran$BIC)])
      Stat_Baran[i,"Baran_AIC"]<-min(test_Baran$AIC[!is.na(test_Baran$AIC)])
    },
    error = function(e){
      message('Caught an error!')
      print(e)
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
    },
    finally = {
      message('All done, quitting.')
    })
  
}
toc()

 # Stats Comparison #
  Compare_AIC<-data.frame(Subset =  1:max(DF$ID),
                        Poly_AICc = Stat_Poly["Poly_AIC"],
                        Gromp_AICc = Stat_Gromp["Gromp_AIC"],
                        Barn_AICc = Stat_Baran["Baran_AIC"])

  Compare_AICc<-data.frame(Subset =  1:max(DF$ID),
                         Poly_AICc = Stat_Poly["Poly_AICc"],
                         Gromp_AICc = Stat_Gromp["Gromp_AICc"],
                         Barn_AICc = Stat_Baran["Baran_AICc"])

  Compare_BIC<-data.frame(Subset =  1:max(DF$ID),
                        Poly_BIC = Stat_Poly["Poly_BIC"],
                        Gromp_BIC = Stat_Gromp["Gromp_BIC"],
                        Barn_BIC = Stat_Baran["Baran_BIC"])

  Best_model<-data.frame(Subset = 1:max(DF$ID),
                       BestModelAIC = "NA",
                       AIC ="NA",
                       BestModelAICc = "NA",
                       AICc ="NA",
                       BestModelBIC = "NA",
                       BIC="NA")
  Stat_Gromp[i,"Gromp_AICc"]<-min(test_Gromp$AICc[!is.na(test_Gromp$AICc)])
  Stat_Gromp[i,"Gromp_BIC"]<- min(test_Gromp$BIC[!is.na(test_Gromp$BIC)])
  Stat_Gromp[i,"Gromp_AIC"]<-min(test_Gromp$AIC[!is.na(test_Gromp$AIC)])
  Stat_Baran[i,"Baran_AICc"]<-min(test_Baran$AICc[!is.na(test_Baran$AICc)])
  Stat_Baran[i,"Baran_BIC"]<- min(test_Baran$BIC[!is.na(test_Baran$BIC)])
  Stat_Baran[i,"Baran_AIC"]<-min(test_Baran$AIC[!is.na(test_Baran$AIC)])


  for (i in 1:max(DF$ID)) {
    
    if (Compare_AIC[i, "Gromp_AIC"]  == 0 && Compare_AIC[i, "Baran_AIC"] == 0){
      Best_model[i,"AIC"]<- Compare_AIC[i, "Poly_AIC"]
      Best_model[i,"BestModelAIC"] <- "Cubic Poly"
    } else if (Compare_AIC[i, "Poly_AIC"] < Compare_AIC[i, "Gromp_AIC"] && Compare_AIC[i, "Poly_AIC"] < Compare_AIC[i, "Baran_AIC"]){
      Best_model[i,"AIC"]<- Compare_AIC[i, "Poly_AIC"]
      Best_model[i,"BestModelAIC"] <- "Cubic Poly"
    }  else if (Compare_AIC[i, "Gromp_AIC"] < Compare_AIC[i, "Poly_AIC"] && Compare_AIC[i, "Gromp_AIC"] < Compare_AIC[i, "Baran_AIC"]){
      Best_model[i,"AIC"]<- Compare_AIC[i, "Gromp_AIC"]
      Best_model[i,"BestModelAIC"] <- "Grompertz"
    } else if (Compare_AIC[i, "Baran_AIC"] < Compare_AIC[i, "Poly_AIC"] && Compare_AIC[i, "Baran_AIC"] < Compare_AIC[i, "Gromp_AIC"]){
      Best_model[i,"AIC"]<- Compare_AIC[i, "Baran_AIC"]
      Best_model[i,"BestModelAIC"] <- "Baranyi"
    }
    
    if (Compare_AICc[i, "Gromp_AICc"]  == 0 && Compare_AICc[i, "Baran_AICc"] == 0){
      Best_model[i,"AICc"]<- Compare_AICc[i, "Poly_AICc"]
      Best_model[i,"BestModelAICc"] <- "Cubic Poly"
    } else if (Compare_AICc[i, "Poly_AICc"] < Compare_AICc[i, "Gromp_AICc"] && Compare_AICc[i, "Poly_AICc"] < Compare_AICc[i, "Baran_AICc"]){
      Best_model[i,"AICc"]<- Compare_AICc[i, "Poly_AICc"]
      Best_model[i,"BestModelAICc"] <- "Cubic Poly"
    }  else if (Compare_AICc[i, "Gromp_AICc"] < Compare_AICc[i, "Poly_AICc"] && Compare_AICc[i, "Gromp_AICc"] < Compare_AICc[i, "Baran_AICc"]){
      Best_model[i,"AICc"]<- Compare_AICc[i, "Gromp_AICc"]
      Best_model[i,"BestModelAICc"] <- "Grompertz"
    } else if (Compare_AICc[i, "Baran_AICc"] < Compare_AICc[i, "Poly_AICc"] && Compare_AICc[i, "Baran_AICc"] < Compare_AICc[i, "Gromp_AICc"]){
      Best_model[i,"AICc"]<- Compare_AICc[i, "Baran_AICc"]
      Best_model[i,"BestModelAICc"] <- "Baranyi"
    }
    
    
    if (Compare_BIC[i, "Gromp_BIC"] == 0 && Compare_BIC[i, "Baran_BIC"] == 0 ){
      Best_model[i,"BIC"]<- Compare_BIC[i, "Poly_BIC"]
      Best_model[i,"BestModelBIC"] <- "Cubic Poly"
    } else if(Compare_BIC[i, "Poly_BIC"] < Compare_BIC[i, "Gromp_BIC"] && Compare_BIC[i, "Poly_BIC"] < Compare_BIC[i, "Baran_BIC"]){
      Best_model[i,"BIC"]<- Compare_BIC[i, "Poly_BIC"]
      Best_model[i,"BestModelBIC"] <- "Cubic Poly"
    } else if (Compare_BIC[i, "Gromp_BIC"] < Compare_BIC[i, "Poly_BIC"] && Compare_BIC[i, "Gromp_BIC"] < Compare_BIC[i, "Baran_BIC"]){
      Best_model[i,"BIC"]<- Compare_BIC[i, "Gromp_BIC"]
      Best_model[i,"BestModelBIC"] <- "Grompertz"
    } else if (Compare_BIC[i, "Baran_BIC"] < Compare_BIC[i, "Poly_BIC"] && Compare_BIC[i, "Baran_BIC"] < Compare_BIC[i, "Gromp_BIC"]){
      Best_model[i,"BIC"]<- Compare_BIC[i, "Baran_BIC"]
      Best_model[i,"BestModelBIC"] <- "Baranyi"
    }
  }

write.csv(Best_model, "../result/BestModel.csv")

 
