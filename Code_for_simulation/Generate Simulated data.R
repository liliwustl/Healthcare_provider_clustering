m=100####m is the sample size
p=2 ##p is the dimension of beta
c1=m*0.1
c2=m*0.2
a1=-1
a2=1
for(T in 1:100){
  set.seed(T)
  ni=round(runif(m,50,100))
  N=sum(ni)
  beta=c(0.2,0.2)
  atrue=c(rep(a1,c1),rep(0,m-c1-c2),rep(a2,c2))
  J1=matrix(rep(1:p,times=p),byrow=FALSE, nrow=p)
  K1=matrix(rep(1:p,times=p),byrow=TRUE, nrow=p)
  sigma=0.2^abs(J1-K1)
  X=mvrnorm(N,rep(0,p),sigma)
  Z=matrix(0,N,m)
  id=NULL
  a=NULL
  SRRtrue=rep(0,m)
  for(k in 1:m){
    Z[(sum(ni[(1:m)<k])+1):(sum(ni[(1:m)<k])+ni[k]),k]=rep(1,ni[k])
    id=c(id,rep(k,ni[k]))
    a=c(a,rep(atrue[k],ni[k]))
  }
  ax=as.vector(Z%*%atrue+X%*%beta)
  Y=rpois(N,exp(ax))##generate poisson model
  Data=as.data.frame(cbind(id,X,Y,a))
  names(Data)=c("id","X1","X2","Y","a")
  write.csv(Data,paste("Simulated_data/poisson-100/Data-",T,".csv",sep=""),row.names=F)
}