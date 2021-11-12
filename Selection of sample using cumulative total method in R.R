###http://home.iitk.ac.in/~shalab/sampling/chapter7-sampling-varying-probability-sampling.pdf
#f1:Sampling Varying probability  without replacement,Des Raj ordered estimator function 
#N:Size of population - e,d:Units(size) - n:size of sample
#yf: Sampling values of Y from sampling - pf:Corresponding probabilities Y from sampling
#x: Units-  y: Y
f1<-function(N,e,d,n){
  pf<-c()
  yf<-c()
  x<-c(e:d)
  y<-c(1:N)
  N=length(y)                                           
  y1<-y                 ## set y to y1 in order to keep the value of N; need N to calculate zi
  #t: Ti
  t<-c(cumsum(x))
  #P:Corresponding probabilities Y(yi)
  P<-c()

  #r:Random number from 1 to length(t)
  q=0
  for (i in 1:length(t)){
    P[i]=x[i]/t[length(t)]
  }
  while (q<n) {
    r<-sample(1:t[length(t)],1,replace=TRUE)                        
    if (r <= t[1]){yf=c(yf,y1[1]);y1=y1[-1];pf=c(pf,P[1]);
                                                                  
    P=P[-1];
    x=x[-1]}else{
      for(i in 2:length(t))
        if(t[i-1] < r & r< t[i]){yf=c(yf,y1[i]);y1=y1[-i];pf=c(pf,P[i]);  
        P=P[-i];
        x=x[-i]}else if (r==t[i]){yf=c(yf,y1[i]);y1=y1[-i];pf=c(pf,P[i]);P=P[-i];x=x[-i]}
    }
    # xi need to change.need to update Ti
    t <- c(cumsum(x))
    q = q+1
  }
  #z: zi(vector)
  z<-c()
  n<-length(yf) # number of samples
  z[1]=(1/N)*(yf[1]/pf[1])
  for(i in 2:n){
    z[i]=(1/N)*(sum(yf[1:i-1])+(yf[i])*((1-(sum(pf[1:i-1]))))/(pf[i])) ##zi=(1/N)*(Y1+Y2+...+(Yi-1)*(1-(p1+...pi-1))/pi)
  }
  #zbar:unbiased estimator for mean YN
  #zv:unbiased estimator for variance YN
  print(z)
  zbar=mean(z)
  zv=(1/n)*(var(z)) ##(1/(n*(n-1))*(sum(zi-zbar)^2)
  a<-c(zbar,zv)
  return(a)
}
#Example
f1(200,51,250,30) #N=200,e=51,d=250,n=30
