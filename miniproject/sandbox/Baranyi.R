rm(list=ls())

DF <- read.csv("/home/frcovell/CMEECourseWork/miniproject/data/ModGrowthData.csv")

test <- DF[ which(DF$ID == 15),]


lm_growth <- lm(log(PopBio) ~ Time, data = subset(DF, ID == 15 & Time > round(min(test$Time)) & Time < test$Time[which.max(diff(diff(log(test$PopBio))))]))
summary(lm_growth) 

coef(summary(lm_growth))["Time","Estimate"]# use to find r_man_start

N_0_start <- min(log(test$PopBio)) # lowest population size, note log scale
K_start <- max(log(test$PopBio)) # highest population size, note log scale
r_max_start <- coef(summary(lm_growth))["Time","Estimate"] # use our previous estimate from the OLS fitting from above
t_lag_start <- test$Time[which.max(diff(diff(log(test$PopBio))))]

baranyi_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  A <- t + 1/ r_max * log(exp(-r_max * t) + exp(-t_lag) - exp(-r_max * t - t_lag))
  return(N_0 + r_max * A - log(1+(exp(r_max * A)-1/ exp(K - N_0 ))))
}  

fit_baranyi <- nlsLM(log(PopBio) ~ baranyi_model(t = Time, r_max, K, N_0, t_lag), test,
                      list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter =100),trace = T)

A <- time + 1/mumax * log(exp(-mumax * time) + exp(-h0) - exp(-mumax * time - h0))
#log_y <- y0 + mumax * A - log(1 + (exp(mumax * A) - 1)/(exp(K - y0)))
log_y <- log(y0) + mumax * A - log(1 + (exp(mumax * A) - 1) / exp(log(K) - log(y0)))