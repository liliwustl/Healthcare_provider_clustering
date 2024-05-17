library(MASS)
library(lme4)
library(nlme)
library(flexclust)
library(dplyr)
library(npmlreg)
Sn=function(x,t){
  xx=1-t/sqrt(sum(x^2))
  if(xx>0){
    s=xx*x
  }else{
    s=0*x
  }
  list(s=s)
}


Theta_SCAD=function(u,lambda){
  L=length(u)
  Ta=rep(0,L)
  for(k in 1:L){
    uabs=abs(u[k])
    if(uabs<=(lambda+lambda/eta)){
      Ta[k]=Sn(x=u[k],t=lambda/eta)$s
    }else if(uabs>(lambda+lambda/eta)&uabs<=(v*lambda)){
      Ta[k]=Sn(x=u[k],t=v*lambda/((v-1)*eta))$s/(1-1/((v-1)*eta))
    }else{
      Ta[k]=u[k]
    }
  }
  list(Ta=Ta)
}

Fn_SCAD=function(lambda){
  ahat=a0
  betahat=b0
  theta=theta0
  upsilon=upsilon0
  counter=1
  eps_rel=0.0001
  eps_abs=0.0001
  repeat{
    a1=as.vector(Z%*%ahat+X%*%betahat)
    phi=sum((Y-exp(a1))^2/exp(a1))/(N-p-m)
    W=diag(exp(a1)/phi)
    Yt=Z%*%ahat+X%*%betahat+(1/exp(a1))*(Y-exp(a1))
    xx=solve(t(X)%*%W%*%X)
    Qx=diag(N)-X%*%xx%*%t(X)%*%W
    ahat=solve(t(Z)%*%W%*%Qx%*%Z+eta*t(Delta)%*%Delta)%*%(t(Z)%*%W%*%Qx%*%Yt+eta*t(Delta)%*%(theta-1/eta*upsilon))
    betahat=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%(Yt-Z%*%ahat)
    theta1=theta
    for(i in 1:(m-1)){
      Pi=ahat[i]-ahat[(1:m)>i]+1/eta*upsilon[((i-1)*(2*m-i)/2+1):((i-1)*(2*m-i)/2+m-i)]
      theta[((i-1)*(2*m-i)/2+1):((i-1)*(2*m-i)/2+m-i)]=Theta_SCAD(Pi,lambda)$Ta
      upsilon[((i-1)*(2*m-i)/2+1):((i-1)*(2*m-i)/2+m-i)]=upsilon[((i-1)*(2*m-i)/2+1):((i-1)*(2*m-i)/2+m-i)]+eta*(ahat[i]-ahat[(1:m)>i]-theta[((i-1)*(2*m-i)/2+1):((i-1)*(2*m-i)/2+m-i)])
    }
    r=Delta%*%ahat-theta
    s=eta*t(Delta)%*%(theta-theta1)
    primal=sqrt(sum(r^2))
    dual=sqrt(sum(s^2))
    if(5*primal>dual&counter%%5==0){
      eta=2*eta
    }else{
      eta=eta
    }
    eps_p=eps_abs*sqrt(m*(m-1)/2)+eps_rel* max(sqrt(sum((Delta%*%ahat)^2)),sqrt(sum((theta)^2)))
    eps_d=eps_abs*sqrt(m)+eps_rel*sqrt(sum((t(Delta)%*%upsilon)^2))
    if(primal<eps_p&dual<eps_d){
      break
    }
    if(counter>300){
      break
    }
    counter=counter+1
  }
  list(ahat=ahat,betahat=betahat,phi=phi)
}
eta=1
v=3
T=1
data=read.csv(paste('Simulated_data/poisson-50/Data-',T,'.csv',sep=""))
Y=data$Y
atrue=(data%>%group_by(id)%>%slice(1))$a
rate=1-sum(Y)/length(Y)
id=data$id
N=dim(data)[1]
p=dim(data)[2]-3
XX=cbind(data$X1,data$X2)
X=scale(XX)
m=id[N]
ni=rep(0,m)
for(j in 1:m){
  ni[j]=length(which(id==j))
}
Z=matrix(0,N,m)
for(k in 1:m){
  if(k==1){
    Z[(1:ni[1]),1]=rep(1,ni[1])
  }else{
    Z[(sum(ni[(1:m)<k])+1):(sum(ni[(1:m)<k])+ni[k]),k]=rep(1,ni[k])
  }
}
Im=diag(m)
Delta=NULL
for(i in 1:(m-1)){
  Delta=cbind(Delta,(Im[,i]-Im[,(1:m)>i]))
}
Delta=t(Delta)
xi=c(0.21,0.23,0.25,0.27,0.3)
nxi=length(xi)
  #####fused effects model####
  fix=glm(Y~-1+X+factor(id),family=quasipoisson,data=data)
  afix=as.vector(coef(fix)[-(1:p)])
  betafix=coef(fix)[1:p]
  a0=as.vector(afix)
  b0=as.matrix(as.numeric(betafix))
  up0=rep(0,m)
  theta0=NULL
  upsilon0=NULL
  for(i in 1:(m-1)){
    theta0=c(theta0,(a0[i]-a0[(1:m)>i]))
    upsilon0=c(upsilon0,(up0[i]-up0[(1:m)>i]))
  }
  Group=rep(0,nxi)
  bic=rep(0,nxi)
  b1=rep(0,nxi)
  b2=rep(0,nxi)
  ascad=list()
  betascad=list()
  Phi=rep(0,nxi)
  for(i in 1:nxi){
    value=Fn_SCAD(lambda=xi[i])
    ascad[[i]]=value$ahat
    betascad[[i]]=value$betahat
    Phi[i]=value$phi
    ai=ascad[[i]]
    j=1
    group=list()
    group[[1]]=which(abs(ai-ai[1])<0.2)
    while(length(unlist(group))<m){
      ai[-unlist(group)][which(abs(ai[-unlist(group)]-ai[-unlist(group)][1])<0.2)]=j
      group[[j+1]]=which(ai==j)
      j=j+1
    }
    mu=as.numeric(lapply(1:length(group),function(x)1/length(group[[x]])*sum(ascad[[i]][group[[x]]])))
    df=cbind(lengths(group),mu)
    df=df[order(df[,2]),]
    print(df)
    Group[i]=length(group)
    b1[i]=-2*sum(Y*(Z%*%ascad[[i]]+X%*%betascad[[i]])-exp(Z%*%ascad[[i]]+X%*%betascad[[i]]))
    Cn=log(log(N+p))
    b2[i]=log(N)*(Group[i]+p)
    bic[i]=b1[i]+Cn*b2[i]
  }
  I=which.min(bic)
  aI=ascad[[I]]
  j=1
  group0=list()
  group0[[1]]=which(abs(aI-aI[1])<0.2)
  while(length(unlist(group0))<m){
    aI[-unlist(group0)][which(abs(aI[-unlist(group0)]-aI[-unlist(group0)][1])<0.2)]=j
    group0[[j+1]]=which(aI==j)
    j=j+1
  }
  mu0=as.numeric(lapply(1:length(group0),function(x)1/length(group0[[x]])*sum(ascad[[I]][group0[[x]]])))
  group=lapply(order(mu0),function(x)group0[[x]])
  mu=mu0[order(mu0)]
  df=cbind(lengths(group),mu)
  print(df)
  ngroup=length(group)
  id1=rep(0,N)
  Z1=matrix(0,m,ngroup)
  for(k in 1:ngroup){
    Z1[group[[k]],k]=1
    id1[unlist(lapply(group[[k]],function(x)(sum(ni[1:x])-ni[x]+1):sum(ni[1:x])))]=k
  }
  data1=data.frame(X,Y,id1)
  model1=glm(Y~-1+X+factor(id1),family=quasipoisson,data=data1)
  para=as.numeric(summary(model1)$coef[,1])
  se=as.numeric(summary(model1)$coef[,2])
  aest=Z1%*%para[-(1:p)]
betaest=para[1:p]
se.beta=se[1:p]
Aest=para[-(1:p)]
se.a=se[-(1:p)]
aMSE=sum((atrue-aest)^2)/m
RI=comPart(round(as.numeric(aest)),round(atrue))

#Oracle
aor=rep(0,m)
gk=list()
id_or=rep(0,N)
level=levels(factor(atrue))
for(j in 1:length(level)){
 gk[[j]]=which(atrue==level[j])
 id_or[unlist(lapply(gk[[j]],function(t)which(id==t)))]=j
}
data2=data.frame(X,Y,id_or)
model_or=glm(Y~-1+X+factor(id_or),family=poisson,data=data2)
summary(model_or)
para_or=as.numeric(coef(model_or))
se_or=as.numeric(summary(model_or)$coef[,2])
betaor=para_or[1:p]
for(j in 1:length(level)){
  aor[gk[[j]]]=para_or[p+j]
}
se.beta_or=se_or[1:p]
aMSE_or=sum((atrue-aor)^2)/m
Aor=para_or[-(1:p)]
se.a_or=se_or[-(1:p)]


#random effect
model=glmer(Y~X-1+(1|factor(id)),family=poisson)
betaran=as.numeric(coef(model)$"factor(id)"[1,-1])
aran=coef(model)$"factor(id)"[,1]
aMSE_ran=sum((atrue-aran)^2)/m
se.beta_ran=summary(model)$coef[,2]


#fixed effect
fix=glm(Y~-1+X+factor(id),family=poisson)
afix=as.vector(coef(fix)[-(1:p)])
betafix=coef(fix)[1:p]
aMSE_fix=sum((atrue-afix)^2)/m
se.beta_fix=summary(fix)$coef[1:p,2]


###latent class ######
Group=c(2,3,4,5,6,7)
fit1=allvc(Y~X1+X2,random=~1|id,random.distribution="np",k=Group[1],
           family=poisson(link=log),data=data)
fit2=allvc(Y~X1+X2,random=~1|id,random.distribution="np",k=Group[2],
           family=poisson(link=log),data=data)
fit3=allvc(Y~X1+X2,random=~1|id,random.distribution="np",k=Group[3],
           family=poisson(link=log),data=data)
fit4=allvc(Y~X1+X2,random=~1|id,random.distribution="np",k=Group[4],
           family=poisson(link=log),data=data)
fit5=allvc(Y~X1+X2,random=~1|id,random.distribution="np",k=Group[5],
           family=poisson(link=log),data=data)
fit6=allvc(Y~X1+X2,random=~1|id,random.distribution="np",k=Group[6],
           family=poisson(link=log),data=data)
bic1=fit1$disparity+log(log(N+p))*(Group[1]+p)*log(N)
bic2=fit2$disparity+log(log(N+p))*(Group[2]+p)*log(N)
bic3=fit3$disparity+log(log(N+p))*(Group[3]+p)*log(N)
bic4=fit4$disparity+log(log(N+p))*(Group[4]+p)*log(N)
bic5=fit4$disparity+log(log(N+p))*(Group[5]+p)*log(N)
bic6=fit4$disparity+log(log(N+p))*(Group[6]+p)*log(N)
bic=c(bic1,bic2,bic3,bic4,bic5,bic6)
ngroup_np=Group[which.min(bic)]
fit=allvc(Y~X1+X2,random=~1|id,random.distribution="np",k=ngroup_np,
          family=poisson(link=log),data=data)
betanp=coef(fit)[1:p]
se.beta_np=summary(fit)$coef[1:p,2]
anp=fit$mass.points[post(fit)$classif]
aMSE_np=sum((atrue-anp)^2)/m
Anp=fit$mass.points
se.a_np=summary(fit)$coef[-(1:p),2]
RI_np=comPart(round(anp),round(atrue))

save_res=list(ngroup=ngroup,ngroup_np=ngroup_np,betaest=betaest,se.beta=se.beta,
              Aest=Aest,se.a=se.a,RI=RI,
              aMSE=aMSE,aMSE_or=aMSE_or,aMSE_fix=aMSE_fix,aMSE_ran=aMSE_ran,aMSE_np=aMSE_np,
              betaran=betaran,se.beta_ran=se.beta_ran,
              betafix=betafix,se.beta_fix=se.beta_fix,
              betaor=betaor,se.beta_or=se.beta_or,
              betanp=betanp,se.beta_np=se.beta_np,
              Aor=Aor,se.a_or=se.a_or,
              Anp=Anp,se.a_np=se.a_np,RI_np=RI_np)
save_name = paste('Results/poisson-50/intermediate_results',T,'.Rdata',sep="")
save(save_res,file = save_name)



