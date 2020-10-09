#AIC
#library(magrittr)

step.m <-stepAIC(m,trace = T)
summary(step.m)
coef(step.m)


pred.m <- as.data.frame(predict(step.m,type = "response",test))
names(pred.m)[1] <- "prediction"
test$prediction.m <- ifelse(pred.m$prediction>0.5,1,0)
c1 <- confusionMatrix(as.factor(test$prediction.m), as.factor(test$Default),positive="1")


