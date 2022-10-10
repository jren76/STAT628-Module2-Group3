library(tidyverse)
library(ggplot2)
bodyfat = read.csv("BodyFat.csv")
attach(bodyfat)

head(bodyfat)
tail(bodyfat)
summary(bodyfat)

df=bodyfat
#df = data.frame(BODYFAT,DENSITY,AGE,WEIGHT,HEIGHT,ADIPOSITY,ABDOMEN,THIGH)
summary(df)

#Histogram of Body Fat %
ggplot(df, aes(x=BODYFAT)) + 
  geom_histogram(color="black",fill="blue") +
  labs(x="Body Fat %",y="Count")
#View Potential Outliers 
df[c(which(BODYFAT==0),which(BODYFAT>40)),]




#Histogram of Age - each input looks to be reasonable
ggplot(df, aes(x=AGE)) + 
  geom_histogram(color="black",fill="blue") +
  labs(x="Age (yrs)",y="Count")




#Histogram of Weight 
ggplot(df, aes(x=WEIGHT)) + 
  geom_histogram(color="black",fill="blue") +
  labs(x="Weight (lbs)",y="Count")
#View Potential Outliers 
df[c(which(WEIGHT>300)),]



#Histogram of Height
ggplot(df, aes(x=HEIGHT)) + 
  geom_histogram(color="black",fill="blue") +
  labs(x="Height",y="Count")
#View Potential Outliers
df[c(which(HEIGHT<50)),]




#Histogram of Adiposity (BMI)
ggplot(df, aes(x=ADIPOSITY)) + 
  geom_histogram(color="black",fill="blue") +
  labs(x="Adiposity (BMI)",y="Count")
#View Potential Outliers
df[c(which(ADIPOSITY>40)),]


#Histogram of others
hist(df$NECK)
hist(df$CHEST)
hist(df$ABDOMEN)
hist(df$HIP)
hist(df$THIGH)
hist(df$KNEE)
hist(df$ANKLE)
hist(df$BICEPS)
hist(df$FOREARM)
hist(df$WRIST)

#View potential outliers
df[df$NECK>50,]
df[df$CHEST>130,]
df[df$ABDOMEN>140,]
df[df$HIP>140,]
df[df$THIGH>80,]
df[df$KNEE>47,]
df[df$ANKLE>30,]
df[df$BICEPS>40,]

#the outliers of ankle are observation 31 and 86
#View ankle
plot(df$ANKLE,df$BODYFAT)
lmmodel = lm(BODYFAT ~ ANKLE,data = df)
abline(lmmodel,col="blue",lwd=5) 
summary(lmmodel)

#Histogram of Abdomen
ggplot(df, aes(x=ABDOMEN)) + 
  geom_histogram(color="black",fill="blue") +
  labs(x="Abdomen (cm)",y="Count")
#View Potential Outliers
df[c(which(ABDOMEN>130)),]



#Histogram of Thigh
ggplot(df, aes(x=THIGH)) + 
  geom_histogram(color="black",fill="blue") +
  labs(x="Thigh (cm)",y="Count")
#View Potential Outliers
df[c(which(THIGH>80)),]




#Recalculate Potential Outlier of Body Fat % Observation 182
obs182_density = df[182,2]
obs182_density
bodyfat182 = ((495/obs182_density) - 450)
bodyfat182
df[182,1] = bodyfat182
df[182,1]
df[182,]




#Recalculate Potential Outlier of Body Fat % Observation 216
obs216_density = df[216,2]
obs216_density
bodyfat216 = ((495/obs216_density) - 450)
bodyfat216
df[216,1] = bodyfat216
df[216,1]
df[216,]




#Recalcuate Potential Outlier of  Height Observation 42
weight_42 = df[42,4]
bmi_42 = df[42,6]
height_42 = sqrt(weight_42*(703/bmi_42))
df[42,5] = height_42
df[42,]


#By observing the potential outliers, the gentleman corresponding to ID Number 39 appears to be a mistake.
#Measurements such as abdomen and thigh cannot be recalulate using other variables so we will take out that observation.
bodyfat_new = df[-c(39,182,216),]
dim(bodyfat_new) #sanity check
summary(bodyfat_new)


write.csv(bodyfat_new,file = "Clean_data.csv")


