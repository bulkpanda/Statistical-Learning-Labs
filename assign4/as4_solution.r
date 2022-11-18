#1==================================================================================
library(nnet)
library(MASS)
wine = read.csv2("winequality-white.csv")
wine$quality = factor(wine$quality)
for(i in 1:11){
  wine[,i]=as.double(wine[,i])
}
str(wine)

#a--------------------
levels(wine$quality )=c("bad","bad","average","average","good" , "good","good")
mmod = multinom(quality~.,data=wine)
summary(mmod)

mmodi = step(mmod)
summary(mmodi)
mmodi$coefnames
#b----------------------
omod = polr(quality~.,data = wine)
summary(omod)

omod2 = step(omod)
summary(omod2)  
omod2$coefficients
#c---------------------
newobs = data.frame(fixed.acidity=7.5, volatile.acidity=0.5
                ,citric.acid=0.3,residual.sugar=2.25,chlorides=0.09,
                 free.sulfur.dioxide=14, total.sulphur.dioxide=36,density=0.997,
                 pH=3.3,sulphates=0.63,alcohol=9.5)

predict(mmodi, newobs,type="probs") # prob of being bad = 0.438
predict(omod2, newobs,type="probs") # prob of being bad = 0.446
#d---------------------
summary(omod2)
exp(2*0.28022)     #odds ratio=1.751443

#2==============================================================================
#a-------------

fx=function(x){                       #density function
  b=factorial(11)/(factorial(5)^2)    #normalising constant
  return (b*(x^5)*(1-x)^5)
}

x=seq(0,1,0.01)
y=rep(0,101)
for(i in 1:101){
  y[i]=fx(x[i])
}

rejectionK <- function(fx, a, b, K) {     #rejection algorithm
  while (TRUE) {
    x <- runif(1, a, b)
    y <- runif(1, 0, K)
    if (y < fx(x)) return(x)
  }
}

#b-----------------------------
set.seed(0)                           #set the seed
nreps <- 10000
Observations <- rep(0, nreps)      
for(i in 1:nreps) {
  Observations[i] <- rejectionK(fx, 0, 1, 4)
}
#plot the distribution
hist(Observations, breaks = seq(0, 1, by=0.05), freq = FALSE,
     ylim=c(0, 3), main="Histogram of samples")
lines(x,y)

#3==============================================================================
library(invgamma)
#define parameters
x=c(2,3,4,5,6,7)
y=c(1.9,2.7,4.2,4.8,4.8,5.1)
n=length(x)
J=50000
m=150000
beta_var=10
a=2;b=1

#function to plot the graphs
densityPlot=function(varsample,b0sample,b1sample){
  
  var.median=median(varsample)  #get median of variance
  b0.median=median(b0sample)    #get median of β0
  b1.median=median(b1sample)    #get median of β1
  
  hist(b0sample,breaks=seq(min(b0sample)-1,max(b0sample)+1,0.1),xlim=c(-3,5),ylim=c(0,0.7),freq = F,
       main=expression(Histogram~of~β[0]),xlab = expression(β[0]))
  segments(x0=b0.median,y0=0,x1=b0.median,y1=0.62,col="red",lwd=2)
  text(b0.median,0.65,sprintf("Median = %0.4f",b0.median))
  
  hist(b1sample,breaks=seq(min(b1sample)-1,max(b1sample)+1,0.02),xlim=c(0,1.5),ylim=c(0,3.5),freq = F,
       main=expression(Histogram~of~β[1]),xlab= expression(β[1]))
  segments(x0=b1.median,y0=0,x1=b1.median,y1=3,col="red",lwd=2)
  text(b1.median,3.1,sprintf("Median = %0.4f",b1.median))
  
  hist(varsample,breaks=seq(min(varsample)-1,max(varsample)+1,0.04),xlim=c(0,2.5),ylim=c(0,3),freq = F,
       main=expression(Histogram~of~σ^2), xlab= expression(σ^2))
  segments(x0=var.median,y0=0,x1=var.median,y1=2.5,col="red",lwd=2)
  text(var.median,2.7,sprintf("Median = %0.4f",var.median))
}


gibbs_sampler=function(x,y,n,J,m,beta_var,a,b){
  
  #intital values
  var_i=(a+n/2)*b
  b0_i=sum(y-x)/(n+(var_i/beta_var))
  b1_i=sum(x*(y-b0_i))/(sum(x^2)+(var_i/beta_var))
  
  #initialize the sequences
  var.seq=b0.seq=b1.seq=rep(0,J+m+1)
  var.seq[1]=var_i
  b0.seq[1]=b0_i
  b1.seq[1]=b1_i
  set.seed(0)                 #seed is set to 0
  
  for(j in 2:(J+m+1)){ 
    #for beta0
    k=sum(y-b1.seq[j-1]*x)
    nhat=n+(var.seq[j-1]/beta_var)
    b0mean=k/nhat
    b0var=var.seq[j-1]/nhat
    b0.seq[j]=rnorm(1,mean=b0mean,sd=sqrt(b0var))  
  
    #for beta1
    g=sum(x*(y-b0.seq[j]))
    alpha=sum(x^2)+var.seq[j-1]/beta_var
    b1mean=g/alpha
    b1var=var.seq[j-1]/alpha
    b1.seq[j]=rnorm(1,mean=b1mean,sd=sqrt(b1var))
    
    #for var
    e=sum((y-b0.seq[j]-b1.seq[j]*x)^2)/2
    var.seq[j]=rinvgamma(1,a+n/2,b+e)
  }
  
  #reject first 50000 values
  varsample=var.seq[(J+2):(J+m+1)]
  b0sample=b0.seq[(J+2):(J+m+1)]
  b1sample=b1.seq[(J+2):(J+m+1)]
  
  densityPlot(varsample,b0sample,b1sample)
  
}

gibbs_sampler(x,y,n,J,m,beta_var,a,b)
