y <- 1:10
typeof(y)
class(y)
attributes(y)
length(y)

z <- as.numeric(y)
typeof(z)
class(y)
attribute(z)




m1 <- matrix(1:6, nrow = 2, ncol = 10, byrow = T)

x <- 1:3
length(x)
y <- 10:12
length(y)
s <- cbind(x,y)
typeof(s)
class(s)
attributes(s)

att <- attributes(s)
att$dim
