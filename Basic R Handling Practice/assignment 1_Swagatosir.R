setwd("C:\\Users\\user\\Desktop\\data analytics")
data <- read.csv("Hotel_Review.csv",header=T)
summary(data)
str(data)
table(data$Review_Overall_Rating)
table(data$Review_Type)
table(data$Medium_of_Review)
table(data$Review_Overall_Rating,data$Review_Type)
table(data$Review_Type,data$Medium_of_Review)




#to check whether the Overall Rating of the reviews are different for different review types
#H0 - All review types have similar type of overall review rating
#Ha - Atleast one review type has a different overall review rating than others

a=aov(Review_Overall_Rating~Review_Type,data=data)
summary(a)

#since p value is very low, null goes, alternative stays i.e. we reject the null hypothesis (accept the alternative hypothesis)
#one or more review types have different overall ratings




#to check Whether the Overall Rating of the reviews are different for reviews for differenet reviewer locations
#H0 - All reviewer locations have similar type of overall review rating
#Ha - Atleast one reviewer location has a different overall review rating than others

b=aov(Review_Overall_Rating~Reviewer_Location,data=data)
summary(b)

#since p value is low, null goes, i.e. i.e. we reject the null hypothesis (accept the alternative hypothesis) 
#one or more reviewer locations have different overall ratings




#to check Whether the Overall Rating of the reviews are different for reviews from Bangalore and from outside Bangalore
#H0 - meanBlore=meanOutBlore
#Ha - meanBlore!=meanOutBlore

Blore=data$Review_Overall_Rating[which(data$Reviewer_Location=="bangalore"|data$Reviewer_Location=="Bangalore, India"|data$Reviewer_Location=="Bangalore"|data$Reviewer_Location=="Bangalore, Karnataka, India")]
OutBlore=data$Review_Overall_Rating[-which(data$Reviewer_Location=="bangalore"|data$Reviewer_Location=="Bangalore, India"|data$Reviewer_Location=="Bangalore"|data$Reviewer_Location=="Bangalore, Karnataka, India")]
length(Blore)
length(OutBlore)
t.test(Blore,OutBlore,conf.level = 0.95)
  
#since p value is very high, null stays, i.e. we fail to reject null hypothesis
#overall review rating is not different for reviewer location of Bangalore and ouside Bangalore




#to check Whether review type and medium of the review has any relationship
#H0 - There is no relationship i.e. review type and medium of the review are completely random
#Ha - The review type is related to medium of the review i.e. there is a relationship

c=table(data$Review_Type,data$Medium_of_Review)
c
chisq.test(c)

#since p value is very high, null stays, i.e. we fail to reject null hypothesis
#no relationship is found between review type and medium of review