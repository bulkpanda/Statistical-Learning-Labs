#@Author: Kunal Patel(1291822)
#1==============================================================================

#a--------------------------------
compute_seq=function(x1,r,n,i){
  x=c(x1)
  prev=x1
  for(j in 2:n){                      #generate the sequence
      nextterm=r*prev*(1-prev)
      x=c(x,nextterm)
      prev=nextterm
  }
  for(j in (n-i+1):n){                #print last i values
    print(x[j])
  }
  print("\n")
  plot(seq(1:n),x,xlab = "j",ylab="xj",pch=20)    #plot the values
  title(sprintf("x1 = %0.5f",x1))
}

#b------------------------------
x1seq=c(1/pi,0.5,3/pi)          #the required x1 values
r=0.5
n=4000
i=10
for(x1 in x1seq){ 
  compute_seq(x1,r,n,i)
}

#c----------------------------
r=3
for(x1 in x1seq){ 
  compute_seq(x1,r,n,i)
}
 

#2==============================================================================

#a-------------------------------
getprob=function(i,r,x){              #function to calculate probability values
  prob=choose(i+r-1,r-1)*(x^r)*((1-x)^i)
  return(prob)
}

generate_sample = function(r,x,size){  #function to generate the sample
  n_i=25
  probs=c()
  Fx=c()
  for(j in 0:n_i){                     # getting the Fx(X) distribution.
    probs=c(probs,getprob(j,r,x))
    Fx=c(Fx,sum(probs))
  }
  print(probs)
  
  #x=seq(0,n_i)
  #output=sample(x,size,prob=probs,replace=TRUE) #use sample function
  
  output=c()                    # the sample generation algorithm
  set.seed(71)
  for(j in 1:size){
    
    u=runif(1,0,1)
    z=0
    while(Fx[z+1]<u){
      z=z+1
    }
    output=c(output,z)
  }
  
  return(output)
}

#b--------------------------------
size=100000
sample_output=generate_sample(3,0.6,size) # generate the sample

#plot the probability density
hist1=hist(sample_output,freq = FALSE,xlim=c(0,15),breaks=seq(-0.5, 25.5, by=1),
     ylim=c(0,0.3),main="Probability graph for generated sample")
mean=sum(sample_output)/size     # mean = 1.99993

#c----------------------------------
hist1$density[1:11] #probability values for i=0 to 10



#3==============================================================================
heart=read.csv("heart_failure.csv")  #read the data
str(heart)

#a----------------------------------------
model = glm(cbind(DEATH_EVENT,1-DEATH_EVENT)~.,family = binomial, data=heart)
summary(model) # residual deviance = 219.55 on 286 df
print(model$coefficients)

#b---------------------------------------
heart$diabetes=factor(heart$diabetes)
str(heart)
model2 = glm(cbind(DEATH_EVENT,1-DEATH_EVENT)~.+serum_creatinine*diabetes,family = binomial, data=heart)
summary(model2) #Residual deviance: 217.91  on 285  degrees of freedom

#c-----------------------------------------
# deviance difference in two models will follow chi-square distribution with df=1
pchisq(219.55-217.91,df=1,lower=FALSE)
anova(model,model2,test="Chisq")
# p-value=0.2  test-statistic=1.6421
# high p-value indicates interaction term is not necessary and we can work with common slopes.

#d------------------------------------------
model3=step(model)
summary(model3)

#e-------------------------------------------
newobs=data.frame(age=50,ejection_fraction=30,serum_creatinine=2,serum_sodium=130,
                  time=90)
predict(model3,newobs,type="response") #0.62538

#f-------------------------------------------
pred=predict(model3,heart,type="response")
pred[pred>=0.7]=1
pred[pred<0.7]=0
truevalue=heart$DEATH_EVENT
false_neg=sum(pred&!truevalue)/sum(!truevalue) #0.03448
