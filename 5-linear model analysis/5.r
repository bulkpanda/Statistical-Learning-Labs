library(MASS)
# 1---
fb <- function(n) {
  if (n == 1 || n == 2) {
    return(1)
  } else {
    return(fb(n - 1) + fb(n - 2))
  }
}
fb(8)

#2---
detm = function(a,n){
  d=0
  for(j in 1:n){
    print(j)
    print('*******')
  
    if(n==1){
      return(a)
    }

    else{
      print(a[1,j])
      d=d+(-1)^(1+j)*a[1,j]*detm(a[-1,-j],n-1)
    }
  }
  return(d)
}

a=matrix(c(1,4,3,4,4,7,8,4,6),3,3)
detm(a,3)
det(a)


#3---
#a
mu=c(0,0)
v=matrix(c(1,0.7,1,0.7),2,2)
x=mvrnorm(20,mu=mu, Sigma=v)
x1=x[,1]
y1=x[,2]

#b
model1=lm(y1~x1,x=TRUE)
model1
b0=model1$coefficients[1]
b1=model1$coefficients[2]
ypred1=b0+x1*b1
plot(x1,y1)
lines(x1,ypred1)

x2=c(x1,10)
y2=c(y1,10)
model2=lm(y2~x2,x=TRUE)
model2
b0=model2$coefficients[1]
b1=model2$coefficients[2]
ypred2=b0+x2*b1
plot(x2,y2)
lines(x2,ypred2)

x3=c(x1,10)
y3=c(y1,-0.3)
model3=lm(y3~x3,x=TRUE)
model3
b0=model3$coefficients[1]
b1=model3$coefficients[2]
ypred3=b0+x3*b1
plot(x3,y3)
lines(x3,ypred3)



#c
histhat=function(model){
  xmat=model$x
  H=xmat%*%solve(t(xmat)%*%xmat,t(xmat))
  hat = diag(H)
  writeLines(noquote(paste("Leverage for the model",
                           model$call["formula"],"\n")))
  print(hat)
  hist(hat,main=paste("Leverage for the model",
                      model$call["formula"],"\n"))
  
  
  }
histhat(model1)
histhat(model2)
histhat(model3)

plot(x1,y1,xlim = c(-2.1,11),ylim=c(-2.1,11))
points(10,10,pch=24)
points(10,-0.3,pch=25)
abline(model1,lty=1)
abline(model2,lty=2)
abline(model3,lty=3)


#4---
#a)
ufc= read.csv('ufc.csv')
View(ufc)
heights = ufc$height.m
ufcsortbyheight = ufc[order(-ufc$height.m),]
head(ufcsortbyheight)
ufcsortbywidth = ufc[order(-ufc$dbh.cm),]
head(ufcsortbywidth)

#b
aggregate(x=ufc$dbh.cm, by=list(ufc$species),FUN=mean)
#or
tapply(ufc$dbh.cm, ufc$species, mean)

#c
quantile(ufc$dbh.cm, probs=c(0.25,0.5,0.75))
tapply(ufc$dbh.cm, ufc$species, quantile, probs=c(0.75))
 # DF and WC

#d
