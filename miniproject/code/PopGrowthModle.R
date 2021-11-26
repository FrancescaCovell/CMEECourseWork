## Modeling data

# Required
require(minpack.lm)
require(ggplot2)

# Import data
rm(list=ls())

DF <- read.csv("/home/frcovell/CMEECourseWork/miniproject/data/ModGrowthData.csv")



#### Test models on 1 subset #####
test <- DF[ which(DF$ID == 162),]

plot(test$Time,log(test$PopBio),col='deepskyblue4',xlab='q',main='Observed data')
#lines(test$Time,test$PopBio,col='firebrick1',lwd=3)


## cubic Poly ##

model1 <- lm(log(test$PopBio) ~ poly(test$Time,3, raw = T))
# raw = T use raw and not orthogonal polynomials, gives same output as below equation
#model2 <- lm(log(test$PopBio) ~ test$Time + I(test$Time^2) + I(test$Time^3))


#summary(model2)

predicted.intervals <- predict(model1,data.frame(x=test$Time),interval='confidence',
                               level=0.99)

#predicted.intervals <- predict(model2,data.frame(x=test$Time),interval='confidence',
 #                              level=0.99)

lines(test$Time,predicted.intervals[,1],col='green',lwd=3)
lines(test$Time,predicted.intervals[,2],col='black',lwd=1)
lines(test$Time,predicted.intervals[,3],col='black',lwd=1)
legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)


ggplot(test, aes(x = Time, y = log(PopBio)))+
  geom_point(size = 3) +
  geom_line(aes(x=Time,y=predicted.intervals[,"fit"]))

AIC(model1)
summary(model1)

## Parameters ##
Lag <- test[1:round(length(test$Time)/2),]
Station <-  test[(round(length(test$Time)/2)+1):length(test$Time),]
#Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]
#Station$Time[which.max(diff(diff(log(Station$PopBio))))]

lm_growth <- lm(log(PopBio) ~ Time, data = subset(DF, ID == 21 & Lag$Time[which.max(diff(diff(log(Lag$PopBio))))] & Time <  Station$Time[which.max(diff(diff(log(Station$PopBio))))]))
coef(lm_growth)["Time"]# use to find r_man_start
N_0_start <- min(log(test$PopBio)) # lowest population size, note log scale
K_start <- max(log(test$PopBio)) # highest population size, note log scale
r_max_start <- coef(lm_growth)["Time"] # use our previous estimate from the OLS fitting from above
t_lag_start <- Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]# find last timepoint of lag phase

## gompertz ##
gompertz_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
}   

fit_gompertz <- nlsLM(log(PopBio) ~ gompertz_model(t = Time, r_max, K, N_0, t_lag), test,
                      list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100),trace = T)

summary(fit_gompertz)

AIC(fit_gompertz)

## plotting 
timepoints <- seq(0, max(test$Time), 0.1)

gompertz_points <- gompertz_model(t = timepoints, 
                                  r_max = coef(fit_gompertz)["r_max"], 
                                  K = coef(fit_gompertz)["K"], 
                                  N_0 = coef(fit_gompertz)["N_0"], 
                                  t_lag = coef(fit_gompertz)["t_lag"])

df1 <- data.frame(timepoints, gompertz_points)
df1$model <- "Gompertz model"
names(df1) <- c("Time", "LogN", "model")
ggplot(test, aes(x = Time, y = log(PopBio)))+
  geom_point(size = 3) +
  geom_line(data = df1, aes(x = Time, y = LogN, col = model), size = 1) +
  theme_bw() + # make the background white
  theme(aspect.ratio=1) # make the plot square

## baranyi ##

baranyi_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  A <- t + 1/ r_max * log(exp(-r_max * t) + exp(-t_lag) - exp(-r_max * t - t_lag))
  return(N_0 + r_max * A - log(1+(exp(r_max * A)-1/ exp(K - N_0 ))))
}  

fit_baranyi <- nlsLM(log(PopBio) ~ baranyi_model(t = Time, r_max, K, N_0, t_lag), test,
                     list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100),trace = T)







#### Test on whole dataframe ####

##Cubic Ploy
AIC_Poly <-data.frame(Subset = 1:max(DF$ID),
                      Poly_AIC =0)
for (i in 1:max(DF$ID)) {
  print(i)
  tryCatch(
    expr = {
  test <- DF[ which(DF$ID == i),]
  model1 <- lm(log(test$PopBio) ~ poly(test$Time,3, raw = T))
  AIC_Poly[i,"Poly_AIC"]<-AIC(model1)
  
  predicted.intervals <- predict(model1,data.frame(x=test$Time),interval='confidence',
                                 level=0.99)

  #loop save plots to results
  #temp_plot <- ggplot(test, aes(x = Time, y = log(PopBio)))+
   # geom_point(size = 3) +
    #geom_line(aes(x=Time,y=predicted.intervals[ ,"fit"]))

  #ggsave(temp_plot, file= paste0("/home/frcovell/CMEECourseWork/miniproject/result/plot_", i,".png"))
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

## Groperts

gompertz_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
    return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
 }   
AIC_Gromp <-data.frame(Subset = 1:max(DF$ID),
                      Gromp_AIC =0)
for (i in 1:max(DF$ID)) {
  print(i)
  tryCatch(
  expr = {test <- DF[ which(DF$ID == i),]
  Lag <- test[1:round(length(test$Time)/2),]
  Station <-  test[(round(length(test$Time)/2)+1):length(test$Time),]
  #Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]
  #Station$Time[which.max(diff(diff(log(Station$PopBio))))]
  
  lm_growth <- lm(log(PopBio) ~ Time, data = subset(DF, ID == i & Lag$Time[which.max(diff(diff(log(Lag$PopBio))))] & Time <  Station$Time[which.max(diff(diff(log(Station$PopBio))))]))
  coef(lm_growth)["Time"]# use to find r_man_start
  N_0_start <- min(log(test$PopBio)) # lowest population size, note log scale
  K_start <- max(log(test$PopBio)) # highest population size, note log scale
  r_max_start <- coef(lm_growth)["Time"] # use our previous estimate from the OLS fitting from above
  t_lag_start <- Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]# find last timepoint of lag phase
 
  fit_gompertz <- nlsLM(log(PopBio) ~ gompertz_model(t = Time, r_max, K, N_0, t_lag), data= test,
                        list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100))#, lower = c(0,0,0,0), upper = c(1e+10,1e+10,1e+10,1e+10))#,trace = T)
    summary(fit_gompertz)
    AIC_Gromp[i,"Gromp_AIC"]<-AIC(fit_gompertz)
  
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


## baranyi
baranyi_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  A <- t + 1/ r_max * log(exp(-r_max * t) + exp(-t_lag) - exp(-r_max * t - t_lag))
  return(N_0 + r_max * A - log(1+(exp(r_max * A)-1/ exp(K - N_0 ))))
}  
AIC_Baran <-data.frame(Subset = 1:max(DF$ID),
                      Baran_AIC = 0)

for (i in 1:max(DF$ID)) {
  print(i)
  tryCatch(
  expr = {
  test <- DF[ which(DF$ID == i),]
  Lag <- test[1:round(length(test$Time)/2),]
  Station <-  test[(round(length(test$Time)/2)+1):length(test$Time),]
  #Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]
  #Station$Time[which.max(diff(diff(log(Station$PopBio))))]
  
  lm_growth <- lm(log(PopBio) ~ Time, data = subset(DF, ID == i & Lag$Time[which.max(diff(diff(log(Lag$PopBio))))] & Time <  Station$Time[which.max(diff(diff(log(Station$PopBio))))]))
  coef(lm_growth)["Time"]# use to find r_man_start
  N_0_start <- min(log(test$PopBio)) # lowest population size, note log scale
  K_start <- max(log(test$PopBio)) # highest population size, note log scale
  r_max_start <- coef(lm_growth)["Time"] # use our previous estimate from the OLS fitting from above
  t_lag_start <- Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]# find last timepoint of lag phase
  
  fit_baranyi <- nlsLM(log(PopBio) ~ baranyi_model(t = Time, r_max, K, N_0, t_lag), test,
                     list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100))#,trace = T)

  summary(fit_baranyi)
  AIC_Baran[i,"Baran_AIC"]<-AIC(fit_baranyi)
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






#### Final loop ####
 
 gompertz_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
   return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
 }   

AIC_Gromp <- array("NA", c(1,max(DF$ID))) 
AIC_Poly <- array("NA", c(1,max(DF$ID))) 
for (i in 1:max(DF$ID)) {
  test <- DF[ which(DF$ID == i),]
  
  # Cubic Polynomial #
  model1 <- lm(log(test$PopBio) ~ poly(test$Time,3, raw = T))
  AIC_Poly[,i]<-AIC(model1)
  
  # Gropertz #
  Lag <- test[1:round(length(test$Time)/2),]
  Station <-  test[(round(length(test$Time)/2)+1):length(test$Time),]
  
  lm_growth <- lm(log(PopBio) ~ Time, data = subset(DF, ID == i & Lag$Time[which.max(diff(diff(log(Lag$PopBio))))] & Time <  Station$Time[which.max(diff(diff(log(Station$PopBio))))]))
  coef(lm_growth)["Time"]# use to find r_man_start
  N_0_start <- min(log(test$PopBio)) # lowest population size, note log scale
  K_start <- max(log(test$PopBio)) # highest population size, note log scale
  r_max_start <- coef(lm_growth)["Time"] # use our previous estimate from the OLS fitting from above
  t_lag_start <- Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]# find last timepoint of lag phase
  
  fit_gompertz <- nlsLM(log(PopBio) ~ gompertz_model(t = Time, r_max, K, N_0, t_lag), data= test,
                        list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100))#,trace = T)
  
  AIC_Gromp[,i]<-AIC(fit_gompertz)
  
  #loop save plots to results 
  
  predicted.intervals.Poly <- predict(model1,data.frame(x=test$Time),interval='confidence',
                                 level=0.99)
  predicted.intervals.Gromp <- predict(fit_gompertz,data.frame(x=test$Time),interval='confidence',
                                      level=0.99)
  
  temp_plot <- ggplot(test, aes(x = Time, y = log(PopBio)))+
    geom_point(size = 3) +
    geom_line(aes(x=Time,y=predicted.intervals.Poly[ ,"fit"], color = "red"))+
    geom_line(aes(x=Time,y=predicted.intervals.Gromp))
    
  
  ggsave(temp_plot, file= paste0("/home/frcovell/CMEECourseWork/miniproject/result/plot_", i,".png"))
  
}

#### Broken ####
 ## do a Trycatch
baranyi_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  A <- t + 1/ r_max * log(exp(-r_max * t) + exp(-t_lag) - exp(-r_max * t - t_lag))
  return(N_0 + r_max * A - log(1+(exp(r_max * A)-1/ exp(K - N_0 ))))
}  
AIC_Baran_lim <-data.frame(Subset = 1:max(DF$ID),
                       Baran_AIC ="NA")

for (i in 1:max(DF$ID)) {
  print(i)
  tryCatch(
    expr = {
      test <- DF[ which(DF$ID == i),]
      Lag <- test[1:round(length(test$Time)/2),]
      Station <-  test[(round(length(test$Time)/2)+1):length(test$Time),]
      #Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]
      #Station$Time[which.max(diff(diff(log(Station$PopBio))))]
      
      lm_growth <- lm(log(PopBio) ~ Time, data = subset(DF, ID == i & Lag$Time[which.max(diff(diff(log(Lag$PopBio))))] & Time <  Station$Time[which.max(diff(diff(log(Station$PopBio))))]))
      coef(lm_growth)["Time"]# use to find r_man_start
      N_0_start <- min(log(test$PopBio)) # lowest population size, note log scale
      K_start <- max(log(test$PopBio)) # highest population size, note log scale
      r_max_start <- coef(lm_growth)["Time"] # use our previous estimate from the OLS fitting from above
      t_lag_start <- Lag$Time[which.max(diff(diff(log(Lag$PopBio))))]# find last timepoint of lag phase
      
      fit_baranyi <- nlsLM(log(PopBio) ~ baranyi_model(t = Time, r_max, K, N_0, t_lag), test,
                           list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100), lower = c(0,0,0,0), upper = c(1e+10,1e+10,1e+10,1e+10))#,trace = T)
      
      summary(fit_baranyi)
      AIC_Baran_lim[i,"Baran_AIC"]<-AIC(fit_baranyi)
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


 