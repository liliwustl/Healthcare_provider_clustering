library(MASS)
library(ggplot2)
library(grid)
library (gridExtra)
library(npmlreg)

T=nrep=100
beta=c(0.2,0.2)
p=length(beta)
a1=-1
a2=1
results=matrix(0,16,5)
#############################poisson model with m=50#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/poisson-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==3)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==3)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[1,]=c(kmean,kmedian,ksd,kper,RImean)
results[3,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)


#############################poisson model with m=100#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/poisson-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==3)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==3)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[2,]=c(kmean,kmedian,ksd,kper,RImean)
results[4,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)

#############################logistic model with m=50#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/logistic-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==3)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==3)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[5,]=c(kmean,kmedian,ksd,kper,RImean)
results[7,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)


#############################logistic model with m=100#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/logistic-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==3)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==3)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[6,]=c(kmean,kmedian,ksd,kper,RImean)
results[8,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)

#############################overdispersed poisson model with m=50#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/nbinom-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==3)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==3)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[9,]=c(kmean,kmedian,ksd,kper,RImean)
results[11,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)
#############################overdispersed poisson model with m=100#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/nbinom-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==3)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==3)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[10,]=c(kmean,kmedian,ksd,kper,RImean)
results[12,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)

#############################5 groups with m=50#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/5groups-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==5)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==5)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[13,]=c(kmean,kmedian,ksd,kper,RImean)
results[15,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)

#############################5 groups with m=100#####################################
ngroup=ngroup_np=RI=RI_np=NULL
for(i in 1:nrep){
  res= paste('Results/5groups-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  RI=rbind(RI,save_res$RI)
  RI_np=rbind(RI_np,save_res$RI_np)
}

#SCAD
kmean=mean(ngroup)
kmedian=median(ngroup)
ksd=sd(ngroup)
M=which(ngroup==5)
L=length(M)
kper=L/nrep
RImean=apply(RI,2,mean)[2]
#######latent class#######
kmean_np=mean(ngroup_np)
kmedian_np=median(ngroup_np)
ksd_np=sd(ngroup_np)
M_np=which(ngroup_np==5)
L1=length(M_np)
kper_np=L1/nrep
RImean_np=apply(RI_np,2,mean)[2]

results[14,]=c(kmean,kmedian,ksd,kper,RImean)
results[16,]=c(kmean_np,kmedian_np,ksd_np,kper_np,RImean_np)



method <- c(rep("Poisson with 3 groups SCAD", 2), rep("Poisson with 3 groups LC", 2),
            rep("Logistic with 3 groups SCAD", 2), rep("Logistic with 3 groups LC", 2),
            rep("Overdispersed Poisson with 3 groups SCAD", 2), rep("Overdispersed Poisson with 3 groups LC", 2),
            rep("Poisson with 5 groups SCAD", 2), rep("Poisson with 5 groups LC", 2))
sample_size=rep(c(50,100),4)

colnames(results) <- c("mean", "median", "SD", "per", "RI")

Table1 <- data.frame(Method=method, m=sample_size,results)
sink("Results/Table1.csv",append=FALSE,split=TRUE)
cat("\n### Table 1 in Simulation Results ###\n")
print(Table1)
sink()
