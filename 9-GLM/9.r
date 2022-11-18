
#1-------------------------------------------
#a
library(faraway)
data(wbca)
?wbca
View(wbca)
str(wbca)

#b
model=glm(cbind(wbca$Class,1-Class)~.,family=binomial,data=wbca)
summary(model)

#c
model2 = step(model,scope=~.)

#d
predict(model2,newdata = list(Adhes=1, BNucl=1, Chrom=3,
                              Mitos=1, NNucl=1, Thick=4, UShap=1)
                              ,type = "response")

eta_hat=predict(model2,newdata = list(Adhes=1, BNucl=1, Chrom=3,
                              Mitos=1, NNucl=1, Thick=4, UShap=1)
        ,type = "link",se.fit = TRUE)

c(4.834428-2*0.5815185,4.834428+2*0.5815185)
ilogit(c(4.834428-2*0.5815185,4.834428+2*0.5815185))

#e
pfit=predict(model2,type='response')
falseneg=sum(pfit>=0.5 & !wbca$Class)/sum(!wbca$Class)
falsepos=sum(pfit<0.5&wbca$Class)/sum(wbca$Class)

#f
falseneg2=sum(pfit>=0.9 & !wbca$Class)/sum(!wbca$Class)
falsepos2=sum(pfit<0.9&wbca$Class)/sum(wbca$Class)

#2-----------------------------------------------------

#a
data("pima")
?pima
missing <- with(pima, missing <- glucose==0 | diastolic==0 | triceps==0 | bmi == 0)
pima <- pima[!missing,]

#b
model <- glm(cbind(test, 1-test)~., family=binomial, data=pima)
model2 <- step(model, scope=~.)
summary(model2)

#c
quantile(pima$bmi)

#d
round(cor(pima),3)
round(cor(pima),3)

#e
x <- predict(model2, newdata = list(pregnant=1, glucose=99, bmi=27, diabetes=.25, age=25),
             type="link", se.fit=TRUE)
ilogit(c(x$fit-2*x$se.fit, x$fit, x$fit+2*x$se.fit))

#3-------------------------------------------
data("discoveries")
?discoveries
plot(discoveries)
df=data.frame(year=1860:1959,disc=discoveries)
df
model1=glm(disc~year,family = poisson, df)
summary(model1)
pchisq(164.68-157.32,1,lower.tail = FALSE)

model2=glm(disc~year+I(year^2),family = poisson,df)
pchisq(157.32-132.84,1,lower.tail = FALSE)
summary(model2)
