setwd("C:\\Users\\user\\Desktop\\data analytics\\class4_Saptarshi_Regression")
#Library
library(lattice)
library(ggplot2)
library(caret)
library(MASS)
library(ROCR)
library(gplots)


#Data original
data_or<-read.csv("GermanCredit.csv")

#data_prep
data1 <- data_or
#data1$Owned.Property <- as.factor(data1$Owned.Property)
data1$Age <- as.numeric(data1$Age)
data1$Credit.amount<-as.numeric(data1$Credit.amount)
data1$Credit.amount<-as.factor(ifelse(data1$Credit.amount<=2500,'0-
2500',ifelse(data1$Credit.amount<=5000,'2600-5000','5000+')))

#data1 <- data1[,-c(5)]
set.seed(100)
train_index <- createDataPartition(data1$Default,p=0.8,list=F)
train <- data1[train_index,]
test <- data1[-train_index,]

m<-glm(Default~.,data=train,family=binomial())
#m.step <- stepAIC(m, trace = T)

pred <- as.data.frame(predict(m,type = "response",test))
names(pred)[1] <- "prediction"
test$prediction <- ifelse(pred$prediction>0.5,1,0)

#score test data set
#test$score<-predict(m,type='response',test)
#pred<-prediction(test$score,test$Default)
#perf <- performance(pred,"tpr","fpr")
#plot(perf)
#diff <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

confusionMatrix(as.factor(test$prediction), as.factor(test$Default),positive="1")

