#2------------

#b
income = read.csv('income.csv')
View(income)
x=income[,2]
x=matrix(c(rep(1,15),x),15,2)
y=income[,1]
b=solve(t(x)%*%x,t(x)%*%y)
b

#c
ypred=x%*%b
SSR=t(y-ypred)%*%(y-ypred)
s2=SSR/13
s2

#d
xpoint=c(1,18)
ypoint=xpoint%*%b



#3----------

#a
s=sqrt(s2)
C=solve(t(x)%*%x)
cii=C[2,2]
interval=s*sqrt(cii)
t=(3.017580-2.21929)/(2*interval)
pt(t,13)

#c
xtx = t(x)%*%x
ub = ypoint+sqrt(t(xpoint)%*%C%*%xpoint+1)*s*3.012
lb = ypoint-(t(xpoint)%*%C%*%xpoint+1)*s*3.012



#4--------

cars=c(5.5,5.9,6.5,5.9,8.0,9.0,10.0,10.8)
cost=c(7.2,10.0,9.0,5.5,9.0,9.8,14.5,8.0)
ur=c(8.7,9.4,10.0,9.0,12.0,11.0,12.0,13.7)
ir=c(5.5,4.4,4.0,7.0,5.0,6.2,5.8,3.9)
x=matrix(c(rep(1,8),cost,ur,ir),8,4)
y=cars
b=solve(t(x)%*%x,t(x)%*%y)
ypred=x%*%b
plot(ypred,y)
SSR=t(y-ypred)%*%(y-ypred)
s2=SSR/4

#b
model=lm(cars~cost+ur+ir)
plot(model)
model$coefficients

#c
h=x%*%solve(t(x)%*%x)%*%t(x)
e=y-ypred
z=e[5,1]/sqrt(s2*(1-h[5,5]))
cook=z*z*h[5,5]/(4*(1-h[5,5]))

#d
xpoint=c(1,7,8.6,5)
C=solve(t(x)%*%x)
s=sqrt(s2)
ypoint=xpoint%*%b
range=2.132*s*sqrt(1+t(xpoint)%*%C%*%xpoint)

#e
p=4
n=8
SSRes=sum(e^2)
SSReg=sum(ypred^2)
MSReg=SSReg/p
MSRes=SSRes/(n-p)

Fstat=MSReg/MSRes
qf(0.99,p,n-p)
pf(Fstat,p,n-p,lower.tail = FALSE)

#f
t=(b[3]-1)/(s*sqrt(C[3,3]))
1-pt(t,4)
Fstat2=t^2
pf(Fstat2,1,n-p,lower.tail = FALSE)

#g
#rearrange b1 and b3
x2=matrix(c(cost,ir,rep(1,8),ur),8,4)
C2=solve(t(x2)%*%x2)
A11=C2[1:2,1:2]
Rg1g2=t(c(b[1],b[3]))%*%solve(A11)%*%c(b[1],b[3])
