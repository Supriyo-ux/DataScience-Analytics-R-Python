wine <- read.csv("winequality-white.csv",sep=";")
str(wine)

hist(wine$quality)


wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

library(rpart)

m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart

library(rpart.plot)
rpart.plot(m.rpart, digits = 2)

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

p.rpart <- predict(m.rpart, wine_test)
cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(p.rpart, wine_test$quality)

#model based trees
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)

m.m5p

summary(m.m5p)
p.m5p <- predict(m.m5p, wine_test)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)

#Take it as classification problem
table(wine_train$quality)
wine_train$qualitynew=as.factor(wine_train$quality)
levels(wine_train$qualitynew)=c("others","others","5","6","7","others","others")

wine_test$qualitynew=as.factor(wine_test$quality)
levels(wine_test$qualitynew)=c("others","others","5","6","7","others","others")

wine_train=wine_train[,-12]
m.rpart1 <- rpart(qualitynew ~ ., method="class", data = wine_train)
m.rpart1
rpart.plot(m.rpart1, digits = 3)
p.rpart <- predict(m.rpart1, wine_test, type="class")
table(p.rpart, wine_test$qualitynew)
