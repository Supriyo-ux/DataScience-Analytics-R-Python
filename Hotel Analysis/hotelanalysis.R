setwd("C:\\Users\\user\\Desktop\\data analytics")
data1 <- read.csv("Hotel_Review.csv", header=T)
str(data1)
summary(data1)

#Testing whether average Review_Overall_Rating is equal to 3
# H0 :mean= 3
#Ha: mean!= 3

t.test(data1$Review_Overall_Rating,mu=3,alternative = c("two.sided"),conf.level = 0.95)

#Testing whether average Review_Overall_Rating is higher  than 3.1
# H0 :mean>3.1
#Ha: mean <= 3.1
t.test(data1$Review_Overall_Rating,mu=3.1,alternative = c("greater"),conf.level = 0.95)


#A. Whether the Overall Rating of the reviews are different for different review types?
levels(data1$Review_Type)
fit= aov(Review_Overall_Rating~Review_Type,data = data1)
summary(fit)
#ho is false ha is true

#B. Whether the Overall Rating of the reviews are different for reviews from Bangalore and from outside Bangalore?
levels(data1$Reviewer_Location)

dems <- data1[data1$Reviewer_Location == "Bangalore",]
dems





#C. Whether review type and medium of the review has any relationship?