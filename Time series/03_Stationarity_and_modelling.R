library(TTR)      
library(forecast)    


setwd("C:\\Users\\user\\Desktop\\data analytics\\7.04.19_Time series")

#Moving average
tsdata <- read.csv("Timeseriesdata_7.29.16.csv",header=T)
myvector <- tsdata$EmployedWomen
myts <- ts(myvector,start=c(1964,1),end=c(2016,5),frequency = 12)

plot.ts(myvector, xlab="month" , ylab="EmployedWomen")


training <-tsdata[1:503,] 
#test <- tsdata[504:629,]
test <- tsdata[504:629,]

#recent most 20% can be taken as test data
myvector_training <- training$EmployedWomen
myvector_test <- test$EmployedWomen

#Time Series Creation: Date needs to be taken care of
trainingts <- ts(myvector_training,start=c(1964,1),end=c(2005,11),frequency = 12)
#testts <- ts(myvector_test,start=c(2005,12),end=c(2016,5),frequency = 12)
testts <- ts(myvector_test,start=c(2005,12),end=c(2016,5),frequency = 12)



#library(forecast)
#moving_average = forecast(ma(trainingts, order=2), h=10)

#moving_average_accuracy <- accuracy(moving_average, testts)
#moving_average; moving_average_accuracy
#plot(moving_average)


SMAforecasted <- forecast(SMA(trainingts, n=2), h=126)
WMAforecasted <- forecast(WMA(trainingts, n = 3, wts = 1:3),h=126)
EMAforecasted <- forecast(EMA(trainingts,n=2, ratio =2/3),h=126)

SMAforecasted; WMAforecasted;EMAforecasted
plot(SMAforecasted)
plot(WMAforecasted)
plot(EMAforecasted)

#write.csv(SMAforecasted,"SMAforecasted.csv")
#write.csv(WMAforecasted,"WMAforecasted.csv")



###########Putting h=test set size############
test_size=nrow(test)
SMAforecasted <- forecast(SMA(trainingts, n=3), h=test_size)
WMAforecasted <- forecast(WMA(trainingts, n = 3, wts = 1:3),h=test_size)
EMAforecasted <- forecast(EMA(trainingts,n=3, ratio =2/3),h=test_size)

SMAforecasted; WMAforecasted; EMAforecasted
plot(SMAforecasted)
plot(WMAforecasted)
plot(EMAforecasted)

SMA_df <-as.data.frame(SMAforecasted)
WMA_df <-as.data.frame(WMAforecasted)
EMA_df <-as.data.frame(EMAforecasted)

write.csv(SMAforecasted,"SMAforecasted.csv")
write.csv(WMAforecasted,"WMAforecasted.csv")
write.csv(EMAforecasted,"EMAforecasted.csv")

########Accuracy###################
test$forecast_SMA <- SMA_df$`Point Forecast`
test$forecast_WMA <- WMA_df$`Point Forecast`
test$forecast_EMA <- EMA_df$`Point Forecast`

test$forecast_SMA_diff <- abs(test$EmployedWomen-test$forecast_SMA)/test$EmployedWomen
test$forecast_WMA_diff <- abs(test$EmployedWomen-test$forecast_WMA)/test$EmployedWomen
test$forecast_EMA_diff <- abs(test$EmployedWomen-test$forecast_EMA)/test$EmployedWomen

accuracy_SMA=(1-mean(test$forecast_SMA_diff,na.rm = T))*100
accuracy_WMA=(1-mean(test$forecast_WMA_diff,na.rm = T))*100
accuracy_EMA=(1-mean(test$forecast_EMA_diff,na.rm = T))*100

###################################


