library(ggplot2)


#data load and preparation
setwd("C:\\Users\\user\\Desktop\\data analytics\\neural network")
newyork<-read.csv("NYC_Free_Public_WiFi_03292017.csv",header = T)

attach(newyork)

newyorkdf<-data.frame(newyork$LAT,newyork$LON)

#Determine the number of clusters
wss <- (nrow(newyorkdf)-1)*sum(apply(newyorkdf,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(newyorkdf,
                                     centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#3. K-Means Cluster Analysis
set.seed(20)
fit <- kmeans(newyorkdf, 11) # 11 cluster solution


# get cluster means
aggregate(newyorkdf,by=list(fit$cluster),FUN=mean)

# append cluster assignment
newyorkdf <- data.frame(newyorkdf, fit$cluster)
newyorkdf


newyorkdf$fit.cluster <- as.factor(newyorkdf$fit.cluster)



ggplot(newyorkdf, aes(x=newyork.LON, y=newyork.LAT, color = newyorkdf$fit.cluster)) + geom_point()


devtools::install_github("zachcp/nycmaps")
library(nycmaps)
#map(database="nyc")