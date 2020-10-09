library(randomForest)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

setwd("C:\\Users\\user\\Desktop\\data analytics\\decision tree")


#setwd("D:\\Learning centre\\Coaching\\iitkgp\\DecisionTree\\New folder")

## Read data
termCrosssell<-read.csv(file="bank-full.csv",header = T)
## Explore data frame
names(termCrosssell)

#for(i in 1:10)
#{set.seed(i)
sample.ind <- sample(2, 
                     nrow(termCrosssell),
                     replace = T,
                     prob = c(0.6,0.4))
cross.sell.dev <- termCrosssell[sample.ind==1,]
cross.sell.val <- termCrosssell[sample.ind==2,]



#Stratified sampling
#inTrain <- createDataPartition(data$left, p=0.6, list=FALSE)

#training.data <- data[inTrain,]
#testing.data <- data[-inTrain,]
#dim(training.data)






varNames <- names(cross.sell.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("y")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("y", varNames1, sep = " ~ "))




cross.sell.rf <- randomForest(rf.form,
                              cross.sell.dev,
                              ntree=500,
                              importance=T)

#plot(cross.sell.rf)

# Predicting response variable
cross.sell.val$predicted.response <- predict(cross.sell.rf ,cross.sell.val)

# Create Confusion Matrix
cnf <- confusionMatrix(data=cross.sell.val$predicted.response,
                reference=cross.sell.val$y,
                positive='yes')

out <- cbind.data.frame(1,cnf$byClass[[1]],cnf$byClass[[3]],cnf$overall[[1]])
names(out)[1] <- paste("SampleID")
names(out)[1] <- paste("Accuracy")
names(out)[1] <- paste("Sensitivity")
names(out)[1] <- paste("Precision")



#}








# Variable Importance Plot
varImpPlot(cross.sell.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)



# Variable Importance Plot
varImpPlot(cross.sell.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)


# Variable Importance Table
var.imp <- data.frame(importance(cross.sell.rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
















# Predicting response variable
cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)




# Load Library or packages


## Loading required package: lattice
## Loading required package: ggplot2
# Create Confusion Matrix
confusionMatrix(data=cross.sell.dev$predicted.response,
                reference=cross.sell.dev$y,
                positive='yes')





