# What is the probability that
# one candidate, using random attemps, gives 4 correct ans out of 12
# multiple choice questions each having 5 options
# and 1 correct option

dbinom(4, size=12, prob=0.2)

#find the probability that one candidate gives at max 6 corrent ans

dbinom(0, size=12, prob=0.2) + dbinom(1, size=12, prob=0.2) +
dbinom(2, size=12, prob=0.2) + dbinom(3, size=12, prob=0.2) +
dbinom(4, size=12, prob=0.2) +
dbinom(5, size=12, prob=0.2) + dbinom(6, size=12, prob=0.2)

pbinom(6, size=12, prob=0.2) 

#Find the probability that someone gives 3 or more 
#wrong ans in the above question

#probability of having 6 or less bugs in a script
#with average bugs in scripts being 4
ppois(6, lambda=4)
ppois(6,lambda=4,lower=FALSE) #6 or more

#probability of a student scoring more than 75
#when mean of the class in 60 and standard deviation is 10

pnorm(75, mean=60, sd=10, lower.tail=FALSE) 

#probability of a student scoring less than 75
#when mean of the class in 60 and standard deviation is 10

pnorm(75, mean=50, sd=10, lower.tail=TRUE) 

#random number generation
runif(10, min=1, max=3) #uniform
rbinom(1,size=5,prob=1/2) #binomial
rpois(12,lambda=47) #poisson


