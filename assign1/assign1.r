#4

year=c(75:87)
year
lean=c(642, 644, 656, 667, 673, 688, 696, 698, 713, 717, 725, 742, 757)
lean
df=data.frame(Year=year, Lean=lean)
df
fit=lm(Lean~Year,data=df)
plot(year,lean)

x=matrix(c(rep(1,13),year),13,2)
x

y=lean

b=solve(t(x)%*%x,t(x)%*%y)
b
lean_pred=b[1]+b[2]*year
print(lean_pred)
plot(year,lean, col='red',pch=17)
points(year,lean_pred,col='blue',pch=19,type='b')
legend(75, 700, legend=c("True lean", "Predicted lean"),
       col=c("red", "blue"), lty=c(0,1), cex=0.8, pch = c(17,19))


matplot(year, cbind(lean,lean_pred),type="pl",col=c("red","green"),lty=c(0,0))
