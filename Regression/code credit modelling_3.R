
library(gplots)


l <- seq(0.1, 1, by=0.0005)

for (i in c(1:(length(l)-2)))
  
{
pred.m <- as.data.frame(predict(step.m,type = "response",test))
names(pred.m)[1] <- "prediction"
test$prediction.m <- ifelse(pred.m$prediction>l[i],1,0)
c1 <- confusionMatrix(as.factor(test$prediction.m), as.factor(test$Default),positive="1")
sens <- c1$byClass[1]
one_min_spe <- 1-c1$byClass[2]

if(i==1)
{roc_data <- as.data.frame(cbind(l[i],sens,one_min_spe))
names(roc_data)[1] <- "cutoff"
names(roc_data)[2] <- "sensitivity"
names(roc_data)[3] <- "one_minus_specificity"
}

else
  {roc_data_1 <- as.data.frame(cbind(l[i],sens,one_min_spe))
  names(roc_data_1)[1] <- "cutoff"
  names(roc_data_1)[2] <- "sensitivity"
  names(roc_data_1)[3] <- "one_minus_specificity"
  roc_data <- rbind(roc_data,roc_data_1)
  rm(roc_data_1)
}


}

roc_data_sorted <- roc_data[order(roc_data$one_minus_specificity),]


#ROCR plot
plot(roc_data_sorted$one_minus_specificity,roc_data_sorted$sensitivity,xlab="1-specivity",ylab="sensitivity",
     type="l",main="ROC Curve")

roc_data_sorted$Dist <- roc_data_sorted$sensitivity-roc_data_sorted$one_minus_specificity
roc_data_sorted <- roc_data_sorted[order(-roc_data_sorted$Dist,)]

test$final_prediction <- ifelse(pred.m$prediction>roc_data_sorted[1,1],1,0)
names(test)[24] <- "Final Prediction"

c1 <- confusionMatrix(as.factor(test$final_prediction), as.factor(test$Default),positive="1")

c1$byClass
