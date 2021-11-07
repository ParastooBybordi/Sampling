#Ratio Estimators in Simple Random Sampling  #Parastoo Bybordi
library("MASS")
Sigma <- matrix(c(1,0.9,0.9,1),2,2)  # sigma(x)=sigma(y)=1 , Correlation=0.9 
Sigma
f=mvrnorm(n = 1000, rep(0, 2), Sigma,empirical=TRUE)# mux=muy=0 , N=1000 , empirical=TRUE ;COR=0.9
Y=f[,1]                              # Y
X=f[,2]                              # X
i=sample(1:1000,100)                 # n=100
y=Y[i]
x=X[i]
ybar=mean(y)
xbar=mean(x)
MUx=mean(X)                        
Tx=sum(X)
r=ybar/xbar
MUy=r*MUx                            # MU(hat)y
Ty=r*Tx                              # T(hat)y
total=c(r,MUy,Ty)
print(total)
