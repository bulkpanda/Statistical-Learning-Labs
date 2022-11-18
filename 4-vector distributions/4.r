library(MASS)
#1---
series_sum <- function(x, n) {
  # sum of x^k for k = 0, ..., n
  if (x == 1) {
    return(n + 1)
  } else {
    return((x^(n+1) - 1)/(x - 1))
  }
}

#2---
#The problem is is the line
x[1:n] <- ceiling(10*runif(n))
#One way to fix it is
random.sum.fix <- function(n) {
  # sum of n random numbers
  x <- ceiling(10*runif(n))
  cat("x:", x[1:n], "\n")
  return(sum(x))
}

#3---
#b
sixes=function(n=4){
  for(i in 1:n){
    out=sample(1:6,size=1)
    #print(out)
    if(out==6){
      #print('True')
      return(TRUE)
      stop()
    }
  }
  #print('*')
  return(FALSE)
  }
sixes()

#c
test=function(N){
  win=0
  for(i in 1:N){
    win=win+sixes()
  }
  return(win/N)
}

theory=function(N){
  return(1.0-(5.0/6.0)^N)
}

for(N in c(100,1000,10000)){
  freq=c()
  for(i in 1:10){
    
    freq=c(freq,test(N))
  }
  hist(freq)
}

#d
savedata=function(N=100){
  cat('',file='sixes_sim.txt')
  for(i in 1:N){
    result=sixes()
    cat(result,file='sixes_sim.txt',sep='\n',append=TRUE)
  }
}
N=10000
savedata(N)
data=read.table('sixes_sim.txt',sep='\n')
summary(data)
print(sum(data$V1)/N)

#4---
mu=c(2,4)
v=diag(2)*2
a=0.25*matrix(c(1,1,1,1),2,2)
y=mvrnorm(1000,mu=mu,Sigma=v)
plot(y[,1],y[,2])

#b
quadform=function(y,a) t(y)%*%a%*%y
quadsampleA=hist(apply(y,2,quadform,a),freq=FALSE)
str(quadsampleA)

#c
hist(quadsampleA, col = 'coral1')

#d
chiA = rchisq(n,1,mu%*%A%*%mu)

#e
hist(chiA,add = T, col = 'lightcyan')
# not very nice so improve it a little bit by making the color transparent
# first get the red, green and blue values needed for the rgb() command
col2rgb(c("coral1",'lightcyan'))
hist(quadsampleA, col = rgb(255,114,86,max = 255,alpha = 125))
# because rgb color are defined in range 0-255
hist(chiA, col = rgb(225,255,255,max = 255,alpha = 125),add = T)


