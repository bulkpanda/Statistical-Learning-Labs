#1---------------

data(teengamb)
?teengamb

#backward
fullmodel = lm(gamble~.,data=teengamb)
drop1(fullmodel,scope~.,test="F")
model2 = lm(gamble~sex+income+verbal,data=teengamb)
drop1(model2,scope~.,test='F')
model3 = lm(gamble~sex+income,data=teengamb)
drop1(model3,scope~.,test='F')
#model3 is best


#forward
nullmodel=lm(gamble~1,data = teengamb)
add1(nullmodel,scope = ~.+sex+status+income+verbal,test='F')
model2b=lm(gamble~income,data = teengamb)
add1(model2b,scope=~.+sex+status+verbal,test = 'F')
model3b=lm(gamble~income+sex,data=teengamb)
add1(model3b, scope=~.+status+verbal,test = 'F')


#stepwise
nullmodel=lm(gamble~1,data = teengamb)
step(nullmodel,scope=~.+sex+status+income+verbal)


#2-------------------------
data(trees)
?trees
pairs(trees)
girth=trees$Girth
height=trees$Height
volume=trees$Volume
y=volume
x=cbind(1,girth,height)
b=solve(t(x)%*%x,t(x)%*%y)
b
ssres=sum((x%*%b-y)^2)
trees$Girthsq=trees$Girth^2
model=lm(Volume~Girth+Height+Girthsq+Girthsq*Height,data=trees)
model = step(model,scope = ~.)
plot(model)

filters=read.csv('filters.csv')
View(filters)
filters$type=factor(filters$type)
str(filters)
y=filters$life
x.tr = matrix(0,30,5)
x.tr[,1]=1
for(i in 2:5){
  x.tr[filters$type==i,i]=1
}

#3------

#a
filters <- read.csv("data sets/data/filters.csv")
filters$type <- factor(filters$type)

#b
y <- filters$life
X.treatment <- matrix(0,30,5)
X.treatment [,1] <- 1
# first type is baseline
for (i in 2:5) { X.treatment [filters$type==i,i] <- 1 }

#c
XtXinv <- solve(t(X.treatment )%*%X.treatment )
solve(t(X.treatment )%*%X.treatment )%*%t(X.treatment )%*%y
model <- lm(y~type, data=filters)
(b <- model$coefficients)

#d
(s2 <- sum(lm(y~type, data=filters)$residuals^2)/25)
