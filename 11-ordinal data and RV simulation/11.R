#1---
library(faraway)
library(MASS)
library(nnet)

#a
data(pneumo)
counts <- xtabs(Freq ~ status + year, pneumo)
(props <- prop.table(counts, 2))
years <- c(5.8, 15, 21.5, 27.5, 33.5, 39.5, 46, 51.5)
par(mfrow=c(1,1))
plot(years, props[1,], col="red", ylim=c(0,1))
points(years, props[2,], col="blue")
points(years, props[3,], col="green")
mmod <- multinom(t(counts) ~ years, trace=FALSE)
summary(mmod)
predict(mmod, newdata=list(years=25), type="probs")
pneumo2 <- data.frame(status = rep(pneumo$status, pneumo$Freq),
                        year = rep(pneumo$year, pneumo$Freq))
mmod2 <- multinom(status ~ year, data = pneumo2, trace = FALSE)
summary(mmod2)

#b
pneumo2$status <- ordered(pneumo2$status, levels=c("normal", "mild", "severe"))

omod <- polr(status ~ year, pneumo2)
summary(omod)
#The fit looks good (Figure 2), and the AIC for this model is slightly smaller than that for the
#multinomial logistic regression model, so we prefer it.
plot(years, props[1,], col="red", ylim=c(0,1))
points(years, props[2,], col="blue")
points(years, props[3,], col="green")
fitted <- predict(omod, newdata=list(year=years), type="probs")
lines(years, fitted[,1], col="blue")
lines(years, fitted[,2], col="red")
lines(years, fitted[,3], col="green")
predict(omod, newdata=list(year=25), type="probs")


#3----
F.rand <- function () {
  u <- runif(1)
  x <- 0
  while (F(x) < u) {
    x <- x + 1
  }
  return(x)
}


Fpois <- function(x, la) {
  # poisson(la) cdf at x (assumed integer)
  y <- 0
  py <- exp(-la)
  Fy <- py
  while (y < x) {
    y <- y + 1
    py <- la*py/y
    Fy <- Fy + py
  }
  return(Fy)
}
sapply(0:4, Fpois, la=2) # check it works
## [1] 0.1353353 0.4060058 0.6766764 0.8571
ppois(0:4, 2) # using R' built in function

#To sample from this cdf we can do as following
F.rand <- function(lambda){
  u <- runif(1)
  x <- 0
  p.x <- exp(-lambda)
  5
  F.x <- p.x
  while (F.x < u){
    x <- x + 1
    p.x <- lambda*p.x/x
    F.x <- F.x + p.x
  }
  return(x)
}
n <- 100000
lambda <- 2
F.sample <- rep(0, n)
for (i in 1:n) F.sample[i] <- F.rand(lambda)
table(F.sample)/n
round(dpois(0:9, 2), 5)

#4----

#a
Y.sim <- function() {
  U <- runif(1)
  Y <- 1
  while (U > 1 - 1/(1+Y)) {
    Y <- Y + 1
  }
  return(Y)
}
#1/y(1 + y)

#b
Z.sim <- function() {
  Z <- ceiling(1/runif(1)) - 1
  return(Z)
}






