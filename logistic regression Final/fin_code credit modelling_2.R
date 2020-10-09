#AIC
###Stepwise logistic regression using AIC start###########
step.m <-stepAIC(m,trace = T)
###Stepwise logistic regression using AIC end###########

##Summary
summary(step.m)
coef(step.m)


###Prediction using best model picked by stepAIC and predicting the test data
pred.m <- as.data.frame(predict(step.m,type = "response",test))
###Naming the column of prediction
names(pred.m)[1] <- "prediction.m"

############You need to optimize the cutoff, dont need to execute the code below
test$prediction.m <- ifelse(pred.m$prediction.m>0.5,1,0)
c1 <- confusionMatrix(as.factor(test$prediction.m), as.factor(test$Default),positive="1")


