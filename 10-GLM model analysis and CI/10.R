#1---
library(faraway)
par(mfrow=c(2,2))
plot(1/yield ~ log(nitrogen+1), data=cornnit)
plot(log(yield) ~ log(nitrogen+1), data=cornnit)
plot(yield ~ log(nitrogen+1), data=cornnit)
help(residuals.glm)
par(mfrow=c(2,2))
gmod1 <- glm(yield ~ log(nitrogen+1), data=cornnit, family=Gamma(link="inverse"))
plot(predict(gmod1,type="response"), residuals(gmod1))
gmod2 <- glm(yield ~ log(nitrogen+1), data=cornnit, family=Gamma(link="log"))
plot(predict(gmod2,type="response"), residuals(gmod2))
gmod3 <- glm(yield ~ log(nitrogen+1), data=cornnit, family=Gamma(link="identity"))
plot(predict(gmod3,type="response"), residuals(gmod3))
gmod1$aic
gmod2$aic
gmod3$aic

#a
summary(gmod3)
phihat <- sum(residuals(gmod3, "pearson")^2)/42

#b
anova(gmod3, test="F")
model_dev <- .87603
null_dev <- 2.40614
(F_statistic <- (null_dev - model_dev)/phihat)

#c
par(mfrow=c(2,2))
plot(predict(gmod3, type="link"), residuals(gmod3))
halfnorm(residuals(gmod3), ylab="deviance resid")
halfnorm(rstudent(gmod3), ylab="jackknife resid")
halfnorm(cooks.distance(gmod3), ylab="Cook's distance")

#d
gmod4 <- lm(yield ~ log(nitrogen+1), data=cornnit)
summary(gmod4)

#e
gmod3 <- glm(yield ~ log(nitrogen+1), data=cornnit, family=Gamma(link="identity"))
par(mfrow = c(2,2))
plot(gmod3)
# linear model
gmod4 <- lm(yield ~ log(nitrogen+1), data=cornnit)
par(mfrow = c(2,2))
plot(gmod4)





