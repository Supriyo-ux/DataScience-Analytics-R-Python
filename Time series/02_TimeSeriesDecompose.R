setwd("C:\\Users\\user\\Desktop\\data analytics\\7.04.19_Time series")
#Decompose

tsdata <- read.csv("Timeseriesdata_7.29.16.csv",header=T)
myvector <- tsdata$EmployedWomen
myts <- ts(myvector,start=c(1964,1),end=c(2016,5),frequency = 12)

plot.ts(myvector, xlab="month" , ylab="EmployedWomen")

#Cpmponents of time series
decompose_myts = decompose(myts, "additive")
plot(as.ts(decompose_myts$seasonal))
plot(as.ts(decompose_myts$trend))
plot(as.ts(decompose_myts$random))
plot(decompose_myts)
