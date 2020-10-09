library(alr3)
library(corrplot)
#data(water) ## load the data
#head(water)
library(GGally)


filter.water <- water[,-1]

# Visualize the data 

ggpairs(filter.water) ## It's multivaribale regaression


mlr <- lm(BSAAM~., data = filter.water)
summary(mlr)


data(water)
data()
