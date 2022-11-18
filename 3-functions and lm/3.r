#1

foo=function(x){
  if(x<=0){ y=-x^3}
  if(x>1) {y=sqrt(x)}
  else {y=x^2}
  return(y)
}

foo(4)
# input
x.values <- seq(-2, 2, by = 0.1)
# for each x calculate y
n <- length(x.values)
y.values <- rep(0, n)
for (i in 1:n) {
  x <- x.values[i]
  y=foo(x)
  y.values[i] <- y
}
# output
plot(x.values, y.values, type = "l")

#2

h=function(x,n){
  sum=0
  for(i in 0:n){
    sum=sum+x^i
  }
  return(sum)
}
h(2,3)

#3

h2=function(x,n){
  if(x!=1){
  sum=(1.0-x^(n+1))/(1.0-x)
  }
  else{
    sum=(n+1)*x
  }
  return(sum)
}
h2(1,3)
h2(0.3,55)
h(0.3,55)
h2(6.6,8)
h(6.6,8)


#4

h3=function(x,n){
  sum=0
  i=0
  while(i<=n){
    sum=sum+x^i
    i=i+1
  }
  return(sum)
}

h3(2,3)

h4=function(x,n){
  i=c(0:n)
  vector=x^i
  sum_ = sum(vector)
  return(sum_)
}
h4(6.6,8)


#5

vect=c(4,2)
rotate=function(vect,theta){
  theta=theta*pi/180
  rotation=matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2,2)
  print(rotation)
  new_vect = rotation%*%vect
  return(new_vect)
}
rotate(vect,60)


#6

price=c(50,40,52,47,65)
age=c(1,5,5,10,20)
area=c(1,1,2,2,3)
df=data.frame(
  Price=price,
  Age=age,
  Area=area
)
df
multi.fit=lm(Price~Age+Area,data=df)
summary(multi.fit)
plot(multi.fit)



