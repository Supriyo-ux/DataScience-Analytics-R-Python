setwd("D:\\linearandLogisticSecondbatch")
#Library
library(lattice)
library(ggplot2)
library(caret)
library(MASS)
library(ROCR)
library(gplots)
library(e1071)

#Data original
data_or<-read.csv("GermanCredit.csv")

#creating the copy
data1 <- data_or

#Data manipulation and factor creations (judgemental)
data1$Age <- as.numeric(data1$Age)
data1$Credit.amount<-as.numeric(data1$Credit.amount)
data1$Credit.amount<-as.factor(ifelse(data1$Credit.amount<=2500,'0-
2500',ifelse(data1$Credit.amount<=5000,'2600-5000','5000+')))


#Stratification
set.seed(100)
train_index <- createDataPartition(data1$Default,p=0.8,list=F)
##Training data to build the model
train <- data1[train_index,]

##test data to test the model
test <- data1[-train_index,]

##############Model function start#########
m<-glm(Default~.,data=train,family=binomial())
##############Model function end#########
#m.step <- stepAIC(m, trace = T)

###When you are using stepwise you don't need to execute the bottom part#########
######Prediction###################
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

