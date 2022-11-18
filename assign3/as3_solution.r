#1=====================================================
douglas=read.csv('douglas.csv')
douglas$RootVolume=factor(douglas$RootVolume)
douglas$SeedLot=factor(douglas$SeedLot)
str(douglas)

#a-------------------------
model=lm(Height~RootVolume+SeedLot,data=douglas)
summary(model)

#b-------------------------
heightDiff=model$coefficients[4]-model$coefficients[5]

#c-------------------------
imodel=lm(Height~RootVolume*SeedLot,data=douglas)
summary(imodel)
imodel$coefficients
# get x.tr
x.tr=model.matrix(Height~RootVolume*SeedLot,data=douglas)
tt=c(0,-1,1,0,0,-1,1,0,0) #(B349+RV3+RV3:B349)-(B349+RV2+RV2:B349)
est=tt%*%imodel$coefficients #3.47
n=54
p=9
s2=sum(imodel$residuals^2)/(n-p)
width=qt(0.975,df=n-p)*sqrt(s2*t(tt)%*%solve(t(x.tr)%*%x.tr)%*%tt) #2.03
interval=c(est-width,est+width) #(1.44,5.50)

#d--------------------------------
  #interaction terms must be equal for RV3+RV3:J052 and RV2+RV2:J052
library(car)
C=c(0,-1,1,0,0,0,0,-1,1)
dst=0
linearHypothesis(imodel,C,dst) #p-value = 0.0016
  # we reject the null hypothesis

#e----------------------------------
with(douglas,interaction.plot(RootVolume,SeedLot,Height))
  #lines are not parallel, there may be an interaction

#f-----------------------------------
anova(model,imodel)
  # the p-value for interaction term is 0.4855
  # there is no evidence of interaction

#2====================================================================

#a-------------------------------
ozone=read.csv('ozone.csv')
ozone$Surface=factor(ozone$Surface)
str(ozone)
plot(ozone$UVB[ozone$Surface=="Deep"],ozone$Inhibit[ozone$Surface=="Deep"],col='blue'
     ,pch=16,,xlim = c(0,0.04),xlab = "UVB",ylab="Inhibition(%)")
points(ozone$UVB[ozone$Surface=="Surface"],ozone$Inhibit[ozone$Surface=="Surface"],col='red',pch=17)
legend(0.035, 50, legend=c("Deep", "Surface"),
       col=c("blue", "red"),cex=0.8,pch=c(16,17))

#b------------------------------
  # testing whether interaction is needed
ozimodel=lm(Inhibit~UVB*Surface, data=ozone)
summary(ozimodel)
ozamodel=lm(Inhibit~UVB+Surface, data=ozone)
summary(ozamodel)
anova(ozamodel,ozimodel) # pvalue=0.039
  #interaction is significant => UVB's effect differs at the surface and in the deep.
#other way of doing it (same results)
x.oz=model.matrix(Inhibit~0+Surface,data=ozone)
x=matrix(0,17,4)  # contain only surface and interaction terms
x[,1:2]=x.oz
x[,3]=x[,1]*ozone$UVB
x[,4]=x[,2]*ozone$UVB
y=ozone$Inhibit
ozmodel=lm(y~0+x)
summary(ozmodel)
C=c(0,0,1,-1)
dst=0
linearHypothesis(ozmodel,C,dst) # pvalue=0.039

#3========================================================
#b-------------------------
# randomization
set.seed(1)
x=sample(30)
s1=x[1:5]   # 25,  4,  7,  1,  2
s2=x[6:10]  # 23, 11, 14, 18, 19
s3=x[11:15] # 27, 10, 30, 21, 28
s4=x[16:20] # 9, 5, 22, 15, 12
s5=x[21:30] # 13, 17, 26,  8,  6, 20, 29,  3, 24, 16

#4========================================================
dose=c(0,1,5,15,50,100)
total=c(18,22,22,21,25,28)
tumor=c(0,2,1,4,20,28)
ptumor=tumor/total
#a-----------------------------------------
# function to evaluate log-likelihood
l <- function(tha, ntumors, dose, ntotal) {
  eta <- tha[1] + tha[2]*dose
  return(sum(ntumors*eta - ntotal*log(1 + exp(eta))))
}
#find eta estimates
(betahat <- optim(c(2, 5), l,
            ntumors=tumor, dose=dose, ntotal=total,
            control = list(fnscale = -1,reltol=1e-16))$par)

#plot the curve
d=seq(1,100,1)
p = 1/(1 + exp(-betahat[1] - betahat[2]*d))
plot(dose,ptumor,pch=16)
lines(d,p,col="red",pch=17)
legend(80,0.6,cex=0.9,legend=c("predicted","observed"),col=c("red","black"),lty=c(1,0),pch=c(26,16))

#b---------------------------------------------
# inverse logit function
ilogit <- function(x) 1/(1+exp(-x))

#calculate estimated probabilities from parameters
phat = ilogit(betahat[1]+betahat[2]*dose)

# calculate variance matrix
# substitute estimates for probabilities
I11 <- sum(total*phat*(1 - phat))
I12 <- sum(total*dose*phat*(1 - phat))
I22 <- sum(total*dose^2*phat*(1 - phat))
(Iinv <- solve(matrix(c(I11, I12, I12, I22), 2, 2)))
qnorm(0.975)

#interval for betahat[1]
beta0interval=c(betahat[1]-qnorm(0.975)*sqrt(Iinv[1,1]),betahat[1]+qnorm(0.975)*sqrt(Iinv[1,1])) #(-3.98,-2.09)
#interval for betahat[2]
beta1interval=c(betahat[2]-qnorm(0.975)*sqrt(Iinv[2,2]),betahat[2]+qnorm(0.975)*sqrt(Iinv[2,2])) #(0.0616,0.1186)

#c-----------------------
ylogxy = function(x, y) ifelse(y == 0, 0, y*log(x/y))

#null model
phatN=sum(tumor)/sum(total)
dfN=5
DN=-2*sum(ylogxy(total*phatN, tumor)+ ylogxy(total*(1-phatN), total - tumor))

#our model
D = -2*sum(ylogxy(total*phat, tumor)+ ylogxy(total*(1-phat), total - tumor))
df=4

pchisq(DN - D, 1, lower=FALSE)

#d----------------------
p70 = ilogit(betahat[1]+betahat[2]*70)
xpred=c(1,70)
width=qnorm(0.975)*sqrt(t(xpred)%*%Iinv%*%xpred)
eta_interval=c(betahat[1]+betahat[2]*70-width,betahat[1]+betahat[2]*70+width) #(1.84,4.7)
p70interval=ilogit(eta_interval) #(0.863,0.991)

#e------------


l2 <- function(tha, ntumors, dose, ntotal) {
  eta <- tha[1] + tha[2]*dose
  p=pnorm(eta)
  return(sum(ntumors*log(p/(1-p)) + ntotal*log(1 -p)))
}
(betahat2 <- optim(c(-3, 0.1), l2,
                  ntumors=tumor, dose=dose, ntotal=total,
                  control = list(fnscale = -1,reltol=1e-16))$par)


#f---------------
d=seq(1,100,1)
p2 = pnorm(betahat2[1] + betahat2[2]*d)
p = 1/(1 + exp(-betahat[1] - betahat[2]*d))
plot(dose,ptumor,pch=16)
lines(d,p2,col="red")
lines(d,p,col="blue")
legend(80,0.6,cex=0.9,legend=c("logit","probit","observed"),col=c("red","blue","black"),lty=c(1,1,0),pch=c(26,26,16))

