set.seed(100)
train_index <- createDataPartition(data1$Default,p=0.8,list=F)
train <- data1[train_index,]
test <- data1[-train_index,]



#score test data set
test$score<-predict(step.m,type='response',test)
pred<-prediction(test$score,test$Default)
perf <- performance(pred,"tpr","fpr")
plot(perf)
diff <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

df_cutoff <- cbind.data.frame(perf@x.values,perf@y.values,perf@alpha.values)
names(df_cutoff) <- c("sensitivity","1-specificity","cutoff")
df_cutoff$dist <- abs(df_cutoff$sensitivity-df_cutoff$`1-specificity`)

attach(df_cutoff)
df_cutoff[order(-dist),]
detach(df_cutoff)

df_cutoff_sort <- df_cutoff[order(df_cutoff$dist),]
df_cutoff_sort
