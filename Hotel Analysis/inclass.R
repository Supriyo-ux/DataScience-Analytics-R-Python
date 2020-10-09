setwd("C:\\Users\\user\\Desktop\\data analytics")
data1 <- read.csv("switching.csv", header=T)
str(data1)
summary(data1)

#Testing whether average PSB is equal to 4
# H0 :mean= 4
#Ha: mean!= 4

t.test(data1$PSB,mu=4,alternative = c("two.sided"),conf.level = 0.95)

#Testing whether average PSB is higher  than 4.1
# H0 :mean <=4.1
#Ha: mean > 4.1
t.test(data1$PSB,mu=4.1,alternative = c("greater"),conf.level = 0.95)

#Testing whether average PSB OF RURAL  people is higher thn average psb of urban people
# H0 : meanR > meanU
#Ha: meanR <=menU
URB= data1$PSB[which(data1$UrbanRural=="Urban")]
RUR=data1$PSB[-which(data1$UrbanRural=="Urban")]

t.test(RUR,URB,alternative = c("greater"),conf.level = 0.95)
levels(data1$ProfessionBusiness)
#whether the groups have diff PSB based onprofession

fit= aov(PSB~ProfessionBusiness,data=data1)
summary(fit)

fit=aov(SInt~ProfessionBusiness,data=data1)
summary(fit)

fit1=aov(SInt~ProfessionBusiness + UrbanRural,data=data1)
summary(fit1)

fit2=aov(PSB~ProfessionBusiness * UrbanRural,data=data1)
summary(fit2)

