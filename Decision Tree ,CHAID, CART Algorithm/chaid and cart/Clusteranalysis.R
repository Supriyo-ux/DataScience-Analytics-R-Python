mydata = USArrests
mydata = na.omit(mydata) #listwise deletion
mydata.orig =mydata  #save origin datacopy
mydata= scale(mydata) # standardize variables
d <- dist(mydata, method = "euclidean") #dist matrix
fit <- hclust(d, method = "ward")
plot(fit)

# cut tree into 2 clusters
groups <- cutree(fit, k=4)
# draw dendogram with red borders around the k1 clusters

rect.hclust(fit , k=4, border = "red")

#k-MEAN

# Determine number of clusters #
wss <- (nrow(mydata)-1)*sum(apply(mydata, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers = i)$withinnss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Use optimal no. of clusters in k-means #
k1=4

# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution
aggregate(mydata.orig,by = list(fit$cluster), FUN=mean)

# append cluster assignment
mydata1 <- data.frame(mydata.orig, fit$cluster)
