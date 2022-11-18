#1---
x <- 123
a <- 1.1
b <- 1.2
# a
(z <- x^(a^b))
# b
(z <- (x^a)^b)
# c
(z <- 3*x^3 + 2*x^2 + 6*x + 1) #8 operations
# d
y <- abs(x)
(z <- (y %% 100 - y %% 10)/10)
# e
(z <- z + 1)

#2---
# a
c(1:8, 7:1)
# b
rep(1:5, 1:5)
# c
matrix(1, 3, 3) - diag(3)
# d
matrix(c(0,0,7, 2,5,0, 3,0,0), 3, 3)
#3---
x <- 1:100
idx <- (x %% 2 != 0) & (x %% 3 != 0) & (x %% 7 != 0)
x[idx]

#4---
rm(list = ls())
x <- 1
x[3] <- 3
y <- c()
y[2] <- 2
y[3] <- y[1]
y[2] <- y[4]
z[1] <- 0

#5---
Id <- diag(10)
Id <- 5*Id # one way, using the fact that Id is the identity
Id[Id != 0] <- 5 # another way, using vector indexing of a matrix
diag(Id) <- 5
