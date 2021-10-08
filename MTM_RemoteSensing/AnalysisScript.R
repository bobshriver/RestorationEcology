
library(mvtnorm)

MineDataMessy<-read.csv('Mine_NDVIData.csv') ##This function will read in our dataset
Ref<-0.862180708

  ###Now we will clean up an realign our data###
SP<-MineDataMessy$Mine.Year-1984+1 ###What year into the dataset did mining cease 
MineData<-matrix(NA,36,22)
for (i in 1:22){
  MineData[1:(36-(SP[i]-1)),i]<-as.matrix(MineDataMessy[i,-(1:2)][SP[i]:36])

}
#########3

FitARModelall<-function(){
  parsOut<-matrix(NA,22,2)
  for (i in 1:22){
  mod<-lm(MineData[2:(36-SP[i]+1),i]~MineData[1:(36-SP[i]),i])
  parsOut[i,]<-coef(mod)
  }
  return(parsOut)
}


CIfunction<-function(){
  CIOut<-matrix(NA,22,2)
  for (i in 1:22){
    mod<-lm(MineData[2:(36-SP[i]+1),i]~MineData[1:(36-SP[i]),i])
    Pout<-rmvnorm(1000,coef(mod),vcov(mod))
    CI<-quantile(Pout[,1]/(1-Pout[,2]),probs = c(0.025,.975))
    CIOut[i,]<-CI
  }
  plot(1:22,ylim=c(0,1.5),col='white', ylab='NDVI', xlab='Site')
  abline(h=Ref, col='Red', lwd=2)
  arrows(1:22,y0=CIs[,1],y1=CIs[,2],length=0,lwd=1.5)
  
}
