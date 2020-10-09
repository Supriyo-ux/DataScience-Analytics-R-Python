x <- "dataset"
typeof(x)

attributes(x)

# numeric 
x1 <- as.integer(2.00)
typeof(x1)

x2 <- as.integer(2.74)
typeof(x2)

x3 <- as.numeric(1)
typeof(x3)

x4 <- as.numeric(1.324)
typeof(x4)

x5 <- 1.732
typeof(x5)

a <- TRUE
typeof(a)

#vector
y <- 1:10
typeof(y)
class(y)
attributes(y)
length(y)


z <- as.numeric(y)
typeof(z)
class(z)
attributes(z)


a <- vector()
typeof(a)
#logical
a
#character

b <- vector("character", length = 5)
b

prime_less_10 <- c(2,3,5,11)
length(prime_less_10)
typeof(prime_less_10)
class(prime_less_10)
attributes(prime_less_10)

captain <- c("virat","sourav","dhoni")
z <- c("Sarah", "Tracy", "Jon")

typeof(z)
length(z)


z <- c(z, 1.00)
z
series <- 1:10

seq(10)
p <- seq(from = 1, to = 10, by = 0.1)

seq(100)
p <- seq(from =5, to=100, by=35)


#Other Special Values
1/0
0/0

