m <- matrix(nrow = 2, ncol = 2)
m
dim(m)
m <- matrix(c(1:3))
class(m)

m <- matrix(1:6, nrow = 2, ncol = 3, byrow = T)
m <- matrix(1:6, nrow = 3, ncol = 2, byrow = T)

m      <- 1:10
dim(m) <- c(2, 5)

x <- 1:3
y <- 10:12
cbind(x, y)
class(cbind(x, y))

mdat <- matrix(c(1, 2, 3, 11, 12, 13),
               nrow = 2,
               ncol = 3,
               byrow = F)
mdat
