## Load Titanic library to get the dataset
library(titanic)

## Load the datasets
data("titanic_train")
data("titanic_test")

## Setting Survived column for test data to NA
titanic_test$Survived <- NA

## Combining Training and Testing dataset
complete_data <- rbind(titanic_train, titanic_test)

## Check data structure
str(complete_data)

summary(complete_data)


#understand the missing value pattern
md.pattern(titanic_train)
#plot the missing values
nhanes_miss = aggr(titanic_train, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(titanic_train), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#Drawing margin plot
marginplot(titanic_train[, c("Age", "Survived")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
titanic_train$Sex <- as.factor(titanic_train$Sex)
boxplot(titanic_train[,c("Age", "Survived","Sex")], varwidth = TRUE)

#Loading the mice package
library(mice)
#Loading the following package for looking at the missing values
library(VIM)
library(lattice)
#Imputing missing values using mice
mice_imputes = mice(titanic_train, m=5, maxit = 40)
mice_imputes$method
Imputed_data=complete(mice_imputes,5)

plotdata <- subset(Imputed_data, 
                   select = -c(Name,Ticket,Sex,Cabin,Embarked))
par(mfrow=c(3,3))
for (i in 1:length(plotdata)) {
  boxplot(plotdata[,i], main=names(plotdata[i]), type="l")
  
}
plotdata <- Imputed_data
par(mfrow=c(3,3))
for (i in 1:length(plotdata)) {
  plot(density(plotdata[,i]),main=names(plotdata[i]), type="l")
  #densityplot(plotdata[,i],main=names(plotdata[i]), type="l")
}

ggplot(titanic_train, aes(x=Age, y=Sex)) + geom_boxplot(notch=TRUE)
data("airquality")
md.pattern(airquality)
nhanes_miss = aggr(airquality, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(airquality), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
marginplot(airquality[, c("Ozone", "Solar.R")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
#Imputing missing values using mice
mice_imputes = mice(airquality, m=5, maxit = 40)
mice_imputes$method
Imputed_data=complete(mice_imputes,5)
#Plotting and comparing values with xyplot()
xyplot(mice_imputes, Ozone ~ Solar.R | .imp, pch = 20, cex = 1.4)
#make a density plot
densityplot(mice_imputes)
#Loading the mice package
library(mice)
#Loading the following package for looking at the missing values
library(VIM)
library(lattice)
data(nhanes)
# First look at the data
str(nhanes)
#Convert Age to factor
nhanes$age=as.factor(nhanes$age)
#understand the missing value pattern
md.pattern(nhanes)
#plot the missing values
nhanes_miss = aggr(nhanes, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(nhanes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#Drawing margin plot
marginplot(nhanes[, c("chl", "bmi")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
