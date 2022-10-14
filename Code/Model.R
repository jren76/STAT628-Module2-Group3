setwd("~/Desktop/628/Project 1")
d = read.csv("Clean_data 1.csv")

head(d)
summary(d)
attach(d)
names(d)

## Best subset
if(!require(leaps))  install.packages("leaps")  
library(leaps)
fit =  regsubsets(BODYFAT~AGE+WEIGHT+HEIGHT+
                    ABDOMEN+THIGH+NECK+CHEST+
                    HIP+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = d,nvmax = 13)  

reg.sum = summary(fit)


plot(fit,scale = "adjr2")
plot(fit,scale = "bic")

par(mfrow = c(2,2))
plot(reg.sum$rss,xlab="Number of Variables ",ylab="RSS",type = "l")
plot(reg.sum$adjr2,xlab="Number of Variables ",ylab="adjr2",type = "l")
plot(reg.sum$bic,xlab="Number of Variables ",ylab="bic",type = "l")
plot(reg.sum$cp,xlab="Number of Variables ",ylab="cp",type = "l")


Finalmodel = lm(BODYFAT ~ WEIGHT + ABDOMEN + WRIST,data = d)
summary(Finalmodel)
plot(Finalmodel,c(1,2))

