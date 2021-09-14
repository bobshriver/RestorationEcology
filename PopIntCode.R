annualplant<-function(lambda=1.05,ES=.1,Nstart=1000,years=10,ExTh=100){
  Nall<-matrix(0,years,1000)
  for(i in 1:1000){
    N<-log(Nstart)
    for (t in 1:years){
    if (exp(N)>ExTh){
    N<-N+log(lambda)+rnorm(1,0,ES)
    Nall[t,i]<-exp(N)}
    else{Nall[t,i]<-0}
      
    }

  }
    matplot(Nall,type='l',log='y', ylab="Population Size")
    lines(1:10,exp(apply(log(Nall),1,mean,na.rm=T)), lwd=4)
   plot(1-(apply(Nall>0,1,sum)/1000), xlab="Year", type='l', lwd=2, ylab="Probability of Reaching Quasi-Extinction Threshold", ylim=c(0,1))
 
  }


perenialplant<-function(juvsurv=,ES=1, Nstart=c(1000,0,0),)