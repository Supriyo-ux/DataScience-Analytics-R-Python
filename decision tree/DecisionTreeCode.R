setwd("C:\\Users\\user\\Desktop\\data analytics\\decision tree")


#setwd("D:\\Learning centre\\Coaching\\iitkgp\\DecisionTree\\New folder")


library(rpart)
library(rpart.plot)
library(caret)
library(lattice)
library(ggplot2)
#getwd()
treedata <- read.csv("Decisiontree.csv", header=TRUE)
dim(treedata)

#create data partition
library(caret)
inTrain <- createDataPartition(y=treedata$Quality_Category, p=0.6, list=F)  
training <- treedata[inTrain,]
test <- treedata[inTrain,]
mydata_0<-treedata[which(treedata$Quality_Category=="High"),]
mydata_1<-treedata[which(treedata$Quality_Category=="Low"),]

trainrow_0=floor(nrow(mydata_0)*0.6)
trainrow_1=floor(nrow(mydata_1)*0.6)

set.seed(2)
index_train0=sample(1:nrow(mydata_0),trainrow_0,replace=FALSE)
index_train1=sample(1:nrow(mydata_1),trainrow_1,replace=FALSE)

training0=mydata_0[index_train0,]
training1=mydata_1[index_train1,]

training=rbind(training0,training1) # merge two datasets

test0=mydata_0[-index_train0,]
test1=mydata_1[-index_train1,]

test=rbind(test0,test1)
dim(test)

tree <- rpart(Quality_Category~  fixedacidity + volatileacidity + 
                citricacid + residualsugar + chlorides + freesulfurdioxide + 
                totalsulfurdioxide + density + pH + sulphates + alcohol, 
              data = training, method="class",parms=list(split="gini"))


prp(tree, main="White Wine Quality",
    extra=106, # display prob of survival and percent of obs
    nn=TRUE, # display the node numbers
    fallen.leaves=TRUE, # put the leaves on the bottom of the page
    #shadow.col="gray", # shadows under the leaves
    branch.lty=3, # draw branches using dotted lines
    branch=.5, # change angle of branch lines
    faclen=1, # faclen=0 to print full factor names
    trace=1, # print the automatically calculated cex
    split.cex=1, # make the split text larger than the node text
    split.prefix="is ", # put "is " before split text
    split.suffix="?", # put "?" after split text
    #col=cols, border.col=cols, # green if survived
    split.box.col="lightgreen", # lightgray split boxes (default is white)
    split.border.col="black", # darkgray border on split boxes
    split.round=.5 ,# round the split box corners
    cex=0.6) 
#dev.off()-- 
 # par("mar") # to check figure margin
#par(mar=c(2.5,2.5,2.5,2.5)) # to change figure margin


levels(test$Quality_Category)

pred <- predict(tree, test, type = "class")
levels(pred)

tab <- table(pred, test$Quality_Category) 
print (tab)
1- sum(diag(tab))/sum(tab) # miss-classification error

confusionMatrix(pred, test$Quality_Category)

