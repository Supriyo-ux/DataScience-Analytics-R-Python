setwd("C:\\Users\\user\\Desktop\\data analytics\\7.04.19_Time series")


tsdata <- read.csv("Timeseriesdata_7.29.16.csv",header=T)
myvector <- tsdata$EmployedWomen
myts <- ts(myvector,start=c(1964,1),end=c(2016,5),frequency = 12)
#Frequency
#Annual:1
#Quarterly:4
#Monthly:12
#Weekly:52


###############Example Lag and Lead Start#################################
#set.seed(123)
#x <- sample(c(1:9), 10, replace = T)
#y <- lag(x, 2)

#require(zoo)
library(dplyr)
d <- 1:10
d1 <- lag(d, 2)
d2 <- lead(d, 2)

#x <- zoo(sample(c(1:9), 10, replace = T))

#################Example Lag End##################################

#lagged_ts=lag(myts,k=-2,na.pad=FALSE)

########################ACF#############################
a <- acf(myvector,lag.max = 30)
#acf(myvector,lag.max = 30,type = "partial")
acf(myvector,lag.max = 30)
