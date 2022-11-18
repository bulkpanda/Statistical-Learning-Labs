#1
x=c(1:500)
y=x%%17==0
z=sum(y)

#2
queue <- c("Steve", "Russell", "Alison", "Liam")
queue <- c(queue,"Barry")
queue <- queue[-1]
queue <- c("Pam",queue)
queue <- queue[queue!="Barry"]
queue <- queue[queue!="Alison"]
place = which(queue=="Russell")

#3
patient = data.frame(
  Name=c("Ron", "Steve","Barry","Louise", "Ann","Kristen","Emma"),
  Age=c(23,24,20,30,25,24,21),
  Waiting_time=c(5,7,2,3,5,4,6)

)
print(patient)
patient[which.max(patient$Waiting_time),]

#4
a=matrix(c(2,7,3,4,6,1,0,8,4),3,3)
print(a)
b=matrix(c(1,-1,4,0,-1,0,2,0,1),3,3)
m=a%*%b
print(m)
m2=t(b)%*%a
m2
det_a=det(a)
det_b=det(b)


#5
x1=c(2,4,1)
x2=c(3,0,2)
x3=c(5,-4,6)
a=matrix(cbind(x1,x2,x3),3,3)
z=c(0,1,2)
a=rbind(a,z)
a


#6
x=matrix(c(0,1,2,3,4,5,6,7,8),3,3)
if(NROW(x)!=NCOL(x)){
  print('Not a square matrix')
} else{
  trace = sum(diag(x))
  print(trace)
}
