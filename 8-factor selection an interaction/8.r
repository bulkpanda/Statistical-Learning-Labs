#1----

filters=read.csv('filters.csv')
filters$type=factor(filters$type)

str(filters)
model = lm(life~type, data = filters)
summary(model)
x.tr = matrix(0,30,5)
x.tr[,1]=1
for(i in 2:5){
  x.tr[filters$type==i,i]=1
}

#a
b=solve(t(x.tr)%*%x.tr,t(x.tr)%*%filters$life)
model$coefficients
tt=c(0,0,1,-1,0)
tt%*%b
n=30; p=5
s2=sum(model$residuals^2)/(n-p)
width = qt(0.975,df=25)*sqrt(s2*t(tt)%*%solve(t(x.tr)%*%x.tr)%*%tt)
c(tt%*%b-width,tt%*%b+width)

#c
c =matrix(0,4,5)
c[1,2]=1;c[2,3]=1;c[3,4]=1;c[4,5]=1
(Fstat <- (t(C%*%b) %*% solve(C %*% XtXinv %*% t(C)) %*% C %*% b/4)/s2)
pf(Fstat, 4, 25, lower.tail=FALSE)

#d
dst=rep(0,4)
library(car)
linearHypothesis(model,c,dst)
model.sum = lm(life~type,data=filters,contrasts = list(type='contr.sum'))
linearHypothesis(model.sum,c,dst)

#2=================================================
milk = data.frame(milk=c(18.8,21.2,16.7,19.8,23.9,22.3,15.9,19.2,21.8),
                  diet=factor(c(1,1,2,3,3,1,2,2,3)),
                  breed=factor(c(1,1,1,1,1,2,2,2,2)))

str(milk)

#a
with(milk, interaction.plot(diet, breed, milk))

#b
imodel=lm(milk~breed*diet,data=milk)
summary(imodel)
anova(imodel)

#c
#The degrees of freedom used are 2 and 3

#d
imodel$coefficients
c(1,1,0,1,0,1)%*%imodel$coefficients

#e
amodel=lm(milk~breed+diet,data=milk)
summary(amodel)
heatmap         

#f
linearHypothesis(amodel, c(0,0,1,-1),0)

#g
n <- 9
X <- model.matrix(~breed+diet,data=milk)
y <- milk$milk
XtXinv <- solve(t(X) %*% X)
b <- XtXinv %*% t(X) %*% y
r <- rankMatrix(X)
s2 <- sum((y - X %*% b)^2)/(n - r)
t <- c(1,1,0,1)
mu23 <- t(t) %*% b
wdth <- qt(.975, n - r)*sqrt(s2 * t(t) %*% XtXinv %*% t)
c(mu23 - wdth, mu23, mu23 + wdth)
#[1] 18.82634 22.52222 26.21811
#Use the function estimable
library(gmodels)
help("estimable")
estimable(amodel, c(1,1,0,1), conf.int=0.95)

#h
estimable(imodel, c(1,1,0,1,0,1), conf.int=0.95)

#i
#The second interval is wider than the first because we are attributing some degrees
#of freedom to the interaction term(s). The resulting loss in degrees of freedom for the residuals
#leads to greater error in our estimations
