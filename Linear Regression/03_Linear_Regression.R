library(psych)
setwd("D:/Learning center/Coaching/LinearRegression/lin_reg_share")
getwd()
#Reading the csv file in R
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
#The str() function confirms that the data are formatted as we had expected:
str(insurance)
#Dependent variable is $charges
#Summary calculation (Mean>> Median, here)
summary(insurance$charges)
#Histogram
hist(insurance$charges)
#Correlation Matrix
a <- cor(insurance[c("age", "bmi", "children", "charges")])
a
unique(insurance$region)


#Diagram
pairs(insurance[c("age", "bmi", "children", "charges")])
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

#Training the model Data
ins_model1 <- lm(charges ~ ., data = insurance)
ins_model2 <- lm(charges ~ age+bmi, data = insurance)
ins_model3 <- lm(charges ~ age+bmi+children, data = insurance)
ins_model4 <- lm(charges ~ age+bmi+children+sex, data = insurance)
ins_model5 <- lm(charges ~ age+bmi+children+sex+smoker, data = insurance)
ins_model6 <- lm(charges ~ age+bmi+children+sex+smoker+region, data = insurance)

ins_model1
ins_model2
ins_model4
summary(ins_model1)
summary(ins_model2)
summary(ins_model3)
summary(ins_model4)

