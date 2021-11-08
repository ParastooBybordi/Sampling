library("MASS")
Sigma <- matrix(c(2,(sqrt(2*3))*0.7,(sqrt(2*3))*0.7,3),2,2)  # variance(x)=2,variance(y)=3 , Correlation=0.7 
Sigma
f=mvrnorm(n = 1000, rep(0, 2), Sigma,empirical=TRUE)# mux=muy=0 , N=1000 , empirical=TRUE chun mikhahim COR=0.7 bashad.
Y=f[,1]                              # Y
X=f[,2]                              # X
i=sample(1:1000,100)                 # n=100
y=Y[i]
x=X[i]
#Ratio estimator
ybar=mean(y)
xbar=mean(x)
MUx=mean(X)                        
r=ybar/xbar
MUy=r*MUx                           #mu(hat)y
#Regression estimator
for(j in 1:100){
  b=sum(((y[j]-ybar)*(x[j]-xbar)))/sum((x[j]-xbar)^2) #b=Sxy/sxx
}
MUyL= ybar + b*(MUx - xbar)        #mu(hat)yL
#Difference estimator
MUyD= MUx + (ybar-xbar)            #mu(hat)yD
print(c(MUy,MUyL,MUyD))