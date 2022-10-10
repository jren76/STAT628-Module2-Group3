setwd("~/Desktop/628/Project 1")
d = read.csv("Clean_data 1.csv")

head(d)
summary(d)
attach(d)
names(d)

##STEPAIC
library(MASS)
fullModelFit = lm(BODYFAT~AGE+WEIGHT+HEIGHT+
                    ABDOMEN+THIGH+NECK+CHEST+
                    HIP+KNEE+ANKLE+BICEPS+FOREARM+WRIST) 
summary(fullModelFit)

step = stepAIC(fullModelFit,direction = "both")
step$anova


## Best subset
if(!require(leaps))  install.packages("leaps")  
library(leaps)
fit =  regsubsets(BODYFAT~AGE+WEIGHT+HEIGHT+
                    ABDOMEN+THIGH+NECK+CHEST+
                    HIP+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = d)  

summary(fit)
plot(fit,scale = "r2") 
plot(fit,scale = "adjr2")
plot(fit,scale = "Cp")
plot(fit,scale = "bic")

regsubsets_press(BODYFAT~AGE+WEIGHT+HEIGHT+
                   ABDOMEN+THIGH+NECK+CHEST+
                   HIP+KNEE+ANKLE+BICEPS+FOREARM+WRIST)
