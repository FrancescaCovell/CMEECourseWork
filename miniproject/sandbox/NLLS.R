
rm(list = ls())
graphics.off()
setwd("/home/frcovell/CMEECourseWork/miniproject/code")
library("ggplot2")
library("minpack.lm")
#### Michaelis-menten ####
#V =  V(_max) S / K(_M) + S
##Grenerate data 

S_data <- seq(1,50,5)
S_data

##Generate a Michaelis-Menten reaction velocity response 
#with V_max = 12.5 and K_M = 7.1
V_data <- ((12.5 * S_data)/(7.1 + S_data))
plot(S_data, V_data)

#V max = 12.5 and K M = 7.1 is completely arbitrary. 
#As long as we make sure that V max > 0 , K H > 0 ,
#and K M lies within the lower half of the range of substrate concentrations(0-50)

##Additional random fluctuation

set.seed(1456)
V_data <- V_data + rnorm(10, 0, 1)
plot(S_data, V_data)

##Fitting NLLS
MM_model <- nls(V_data ~ V_max  * S_data / (K_M +S_data))
#start pot not specified adressed later

##Visualisong fit
plot(S_data,V_data, xlab = "Substrate Concentration", ylab = "Reaction Rate") 

lines(S_data,predict(MM_model),lty=1,col="blue",lwd=2)# now overlay the fitted mode

##better visualisation
coef(MM_model) # check the coefficients

#generates ne xaxis value for plotting
Substrate2Plot <- seq(min(S_data), max(S_data), len=200)

#calculate the predicted values by putting fitted coefficients model equation
Predict2Plot <- coef(MM_model)["V_max"] * Substrate2Plot /(coef(MM_model)["K_M"]+
                                                            Substrate2Plot)

#plot
plot(S_data,V_data, xlab = "Substrate Concentration", ylab = "Reaction Rate")

lines(Substrate2Plot, Predict2Plot, lty=1,col="blue",lwd=2)

##retriving statistics 
summary(MM_model)


#### Alometric Scaling ####

MyData <- read.csv("../data/GenomeSize.csv")
head(MyData)#Anisoptera = dragonflies, Zygoptera = Damselflies

Data2Fit <- subset(MyData, Suborder == "Anisoptera")
Data2Fit <- Data2Fit[!is.na(Data2Fit$TotalLength),]# remove NA's


plot(Data2Fit$TotalLength, Data2Fit$BodyWeight, xlab = "Body Length", ylab = "Body
Weight")

dev.off()
ggplot(Data2Fit, aes(x = TotalLength, y = BodyWeight)) +
  geom_point(size = (3),color="red") + theme_bw() +
  labs(y="Body mass (mg)", x = "Wing length (mm)")

## Fitting model

library("minpack.lm")
nrow(Data2Fit)
PowFit <- nlsLM(BodyWeight ~ a * TotalLength^b, data = Data2Fit, 
                start = list(a = .1, b = .1))
powMod <- function(x, a, b) {
  return(a * x^b)
  
}

PowFit <- nlsLM(BodyWeight ~ powMod(TotalLength,a,b), 
                data = Data2Fit, start = list(a = .1, b = .1))

## Visualising fit

Lengths <- seq(min(Data2Fit$TotalLength), max(Data2Fit$TotalLength), len = 200)
coef(PowFit)["a"]
coef(PowFit)["b"]

# Predicted line
Predic2PlotPow <- powMod(Lengths, coef(PowFit)["a"], coef(PowFit)["b"])

# plot
plot(Data2Fit$TotalLength, Data2Fit$BodyWeight)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)

# fit summary

summary(PowFit)

print(confint(PowFit))

#plot residuals 
hist(residuals(PowFit))



#### Exercises ####

# (a) Make the same plot as above, fitted line and all, in ggplot , and add (display) the equation you estimated to your new (ggplot)
#plot. The equation is: Weight = 3.94 × 10 −06 × Length 2.59


#(b) Try playing with the starting values, and see if you can “break” the model fitting – that is, change the starting values till the NLLS
#fitting does not converge on a solution.


#(c) Repeat the model fitting (including a-b above) using the Zygoptera data subset.


#(d) There is an alternative (and in fact, more commonly-used) approach for fitting the allometric model to data: using Ordinary Least
#Squares on bi-logarithamically transformed data. That is, if you take a log of both sides of the allometric equation we get,
#log(y) = log(a) + b log(x)
#This is a straight line equation of the form c = d + bz , where c = log(c) , d = log(a) , z = log(x) , and b is now the slope
#parameter. So you can use Ordinary Least Squares and the linear models framework (with lm() ) in R to estimate the parameters of
#the allometric equation.
#In this exercise, try comparing the NLLS vs OLS methods to see how much difference you get in the parameter estimates between
#them. For example, see the methods used in this paper by Cohen et al 2012.


#(e) The allometry between Body weight and Length is not the end of the story. You have a number of other linear morphological
#measurements ( HeadLength , ThoraxLength , AdbdomenLength , ForewingLength , HindwingLength , ForewingArea , and
#HindwingArea ) that can also be investigated. In this exercise, try two lines of investigation (again, repeated separately for Dragonflies and Damselfiles):

# (i) How do each of these measures allometrically scale with Body length (obtain estimates of scaling constant and exponent)? 
#(Hint: you may want to use the pairs() command in R to get an overview of all the pairs of potential scaling relationships.

#(ii) Do any of the linear morphological measurements other than body length better predict Body weight? That is, does body weight
#Scale more tightly with a linear morphological measurement other than total body length? You would use model selection here,
#which we will learn next. But for now, you can just look at and compare the R 2 values of the models.