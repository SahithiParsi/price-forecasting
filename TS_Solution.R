rm(list = ls(all=T))
setwd("C:/Users/sahithi/Desktop/Cute2/Time series modeling")
data <- read.csv("TS_Train.csv")
val_data <- read.csv("TS_Sample_Submission.csv")

#data preparation 
dim(data)
names(data)
str(data)
head(data)
#missin value check
k<-is.na(data)
sum(k)



#remove outliers
nrow(data)
rows<- seq(1,20,1)
data<- data[-rows,]

#EXPLORATORY DATA ANALYSIS
price<- ts(data$value, frequency = 4 )
pricedecomp<- decompose(price)
plot(pricedecomp)



#VALIDATION OF TREND AND SEASONLITY
par(mfrow=c(1,2)) 
acf(price,lag=30)
pacf(price,lag=30)

plot(price,type="l",lwd=3,col="blue",xlab="Quarter",ylab="price", main="Time series plot")

#test train split
nrow(data)
rows<- seq(1,35,1)
train_data <- data[rows,]
train_data <- data[-rows,]

#test model
hw_price_gamma <- HoltWinters(price, beta=TRUE, gamma=TRUE, seasonal="additive")
head(hw_price_gamma$fitted)

#forecasting  
library('forecast')
hw_price_gamma <- HoltWinters(price, beta=TRUE, gamma=TRUE, seasonal="additive")
head(hw_price_gamma$fitted)
hw_price_forecast <-  forecast(hw_price_gamma,h=7)
test_preds <- data.frame(hw_price_forecast)$Point.Forecast
test_actuals <- val_data$value

library(DMwR)
DMwR::regr.eval(test_actuals,test_preds)
readas.dataframe(test_preds)

