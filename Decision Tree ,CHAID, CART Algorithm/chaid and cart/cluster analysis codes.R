mydata = USArrests 
mydata = na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save orig data copy
mydata = scale(mydata) # standardize variables

d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram

#Remember:
# Ward's method is most appropriate for quantitative variables,
#and not binary variables.

# cut tree into 2 clusters
groups <- cutree(fit, k=4)
# draw dendogram with red borders around the k1 clusters
rect.hclust(fit, k=4, border="red")

#k-MEAN

# Determine number of clusters #
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Look for an "elbow" in the scree plot #

# Use optimal no. of clusters in k-means #
k1=2

# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution

# get cluster means
aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata1 <- data.frame(mydata.orig, fit$cluster)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

# Model Based Clustering
install.packages("mclust")
library(mclust)
fit <- Mclust(mydata)
fit # view solution summary

fit$BIC # lookup all the options attempted
classif = fit$classification # classifn vector
mydata1 = cbind(mydata.orig, classif) # append to dataset
mydata1


fit1=cbind(classif)
rownames(fit1)=rownames(mydata)
library(cluster)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)
