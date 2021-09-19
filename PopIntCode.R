annualplant<-function(lambda=1.15301738,ES=0,perre,years=10,ExTh=40){
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
  par(mfrow=c(1,3))  
  matplot(Nall+1,type='l', ylab="Population Size (Log Scale)",log='y')
  lines(1:years,exp(apply(log(Nall+1),1,mean,na.rm=T)), lwd=4)
  matplot(Nall,type='l', ylab="Population Size")
    lines(1:years,exp(apply(log(Nall+1),1,mean,na.rm=T)), lwd=4)
   plot(1-(apply(Nall>0,1,sum)/1000), xlab="Year", type='l', lwd=2, ylab="Probability of Reaching Quasi-Extinction Threshold", ylim=c(0,1))
 
  }


perennialplant<-function(seedsurv=.2, juvsurv=.4,ES=0, Nstart=c(1000,0,0,0),years=10,ExTh=40, SS=F){
  
  surv<-c(seedsurv,juvsurv,.9,.99) ####Survival rates by size class	
rep<-c(0,.02,3,15) ###Rprodcution rates by size glass


grow<-matrix(0,4,4) ####Growth transition matrix and values
grow[1,1]<-.60
grow[1,2]<-.35
grow[1,3]<-.02
grow[1,4]<-0
grow[2,3]<-.3
grow[2,4]<-.05
grow[2,1]<-.2
grow[2,2]<-.45
grow[3,4]<-.1
grow[3,2]<-.1
grow[3,3]<-.80
grow[4,3]<-.1
grow[4,4]<-.9
grow[1,1]<-.60
grow[2,1]<-.35
grow[3,1]<-.02
grow[4,1]<-0
grow[3,2]<-.3
grow[4,2]<-.05
grow[1,2]<-.2
grow[2,2]<-.45
grow[4,3]<-.1
grow[2,3]<-.1
grow[3,3]<-.80
grow[3,4]<-.1
grow[4,4]<-.9


repmat<-t(rep%o%(grow[,1]))*surv ### Recruitment matrix, Seeds germinate, grow, and then survive
cmat<-t(t(grow)*surv)### Fate of adult plants

A<-repmat+cmat ### Combine Recruitment and Adult plants
if (SS==F){
stagemat<-array(NA,c(years,4,1000)) ###Store output for 100 years 

for(i in 1:1000){ 
n<-Nstart#initialize populations with 100 seeds 
for(t in 1:years){ 
  
  if (sum(c(n))>ExTh){
  Ause<-exp(log(A)+rnorm(1,0,ES))
  #Ause[which(Ause<0,arr.ind = T)]<-0
  n<-Ause%*%n 
  stagemat[t,,i]<-c(n)
  }
  else{stagemat[t,,i]<-0}
}
}
par(mfrow=c(1,3))
matplot(apply(stagemat,c(1,3),sum)+1,type='l',ylab="Population Size (Log Scale)",log='y')
lines(1:years,exp(apply(log(apply(stagemat,c(1,3),sum)+1),1,mean,na.rm=T)), lwd=2)
matplot(apply(stagemat,c(1,3),sum),type='l',ylab="Population Size")
lines(1:years,exp(apply(log(apply(stagemat,c(1,3),sum)+1),1,mean,na.rm=T)), lwd=4)
plot(1-(apply(apply(stagemat,c(1,3),sum)>0,1,sum)/1000), xlab="Year", type='l', lwd=2, ylab="Probability of Reaching Quasi-Extinction Threshold", ylim=c(0,1))
}
if(SS==T){
    n<-Nstart#initialize populations with 100 seeds 
    stagemat<-matrix(NA,years,4) ###Store output for 100 years 
    
    for(t in 1:years){ 
      if (sum(c(n))>ExTh){
        n<-A%*%n 
        stagemat[t,]<-c(n)
      }
      else{stagemat[t,]<-0}
    }
  
  
par(mfrow=c(1,4))  
  barplot(stagemat[1,]/sum(stagemat[1,]),horiz=TRUE, xlim=c(0,2), main="Year 1", xlab='Count',xaxt='n',ylab='Size', col="steelblue",space=0 )
  axis(side=c(1))
  axis(side=2,at=1:4)
  mtext(1,-1)
  barplot(stagemat[2,]/sum(stagemat[2,]),horiz=TRUE , xlim=c(0,2),main="Year 2",xlab='Count',xaxt='n',col="steelblue",space=0  )
  axis(side=1,labels=F) 
  barplot(stagemat[5,]/sum(stagemat[5,]),horiz=TRUE, xlim=c(0,2),main="Year 5" ,xlab='Count' ,xaxt='n',col="steelblue",space=0 )
  axis(side=1,labels=F) 
  barplot(stagemat[10,]/sum(stagemat[10,]),horiz=TRUE, xlim=c(0,2),main="Year 10",xlab='Count',xaxt='n' ,col="steelblue",space=0  )
  axis(side=1,labels=F)   
  
}

}






###########Make Figure##########################

layout(matrix(c(1,2,3,4,5,5,5,5),2,4, byrow = T))
par(mar=c(3,1,4.1,2.1))
barplot(stagemat[1,]/sum(stagemat[1,]),horiz=TRUE, xlim=c(0,2), main="Year 1", xlab='Count',xaxt='n', col="steelblue",space=0 )
axis(side=1,labels=F) 
mtext(1,-1)
barplot(stagemat[2,]/sum(stagemat[2,]),horiz=TRUE , xlim=c(0,2),main="Year 2",xlab='Count',xaxt='n',col="steelblue",space=0  )
axis(side=1,labels=F) 
barplot(stagemat[5,]/sum(stagemat[5,]),horiz=TRUE, xlim=c(0,2),main="Year 5" ,xlab='Count' ,xaxt='n',col="steelblue",space=0 )
axis(side=1,labels=F) 
barplot(stagemat[10,]/sum(stagemat[10,]),horiz=TRUE, xlim=c(0,2),main="Year 10",xlab='Count',xaxt='n' ,col="steelblue",space=0  )
axis(side=1,labels=F) 
par(mar=c(5.1,5.1,1,2.1))
plot(apply(stagemat,1,sum)[1:10], type='l', lwd=3,ylab="Population Density", xlab="Years", cex.lab=1.5, log='y' )
abline(h=apply(stagemat,1,sum)[1], col="red", lty=2,lwd=2)
axis(side=2,labels=F) 


