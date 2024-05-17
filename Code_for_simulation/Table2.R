rm(list=ls())
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
results=matrix(0,37,8)
#############################poisson model with m=50#####################################
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/poisson-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==3){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==3){
   Anp=rbind(Anp,save_res$Anp)
   se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==3)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,0,a2),length(M)),ncol=3,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,0,a2)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==3)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,0,a2),length(M_np)),ncol=3,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,0,a2)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,0,a2),nrep),ncol=3,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,0,a2)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(1,4,7),1:4]=c(abias,asd,ase,acp)
results[c(2,5,8),1:4]=c(abias_or,asd_or,ase_or,acp_or)
results[c(3,6,9),1:4]=c(abias_np,asd_np,ase_np,acp_np)
#############################poisson model with m=100#####################################
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/poisson-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==3){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==3){
    Anp=rbind(Anp,save_res$Anp)
    se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==3)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,0,a2),length(M)),ncol=3,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,0,a2)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==3)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,0,a2),length(M_np)),ncol=3,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,0,a2)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,0,a2),nrep),ncol=3,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,0,a2)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(1,4,7),5:8]=c(abias,asd,ase,acp)
results[c(2,5,8),5:8]=c(abias_or,asd_or,ase_or,acp_or)
results[c(3,6,9),5:8]=c(abias_np,asd_np,ase_np,acp_np)

#############################logistic model with m=50#####################################
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/logistic-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==3){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==3){
    Anp=rbind(Anp,save_res$Anp)
    se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==3)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,0,a2),length(M)),ncol=3,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,0,a2)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==3)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,0,a2),length(M_np)),ncol=3,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,0,a2)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,0,a2),nrep),ncol=3,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,0,a2)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(10,13,16),1:4]=c(abias,asd,ase,acp)
results[c(11,14,17),1:4]=c(abias_or,asd_or,ase_or,acp_or)
results[c(12,15,18),1:4]=c(abias_np,asd_np,ase_np,acp_np)
#############################logistic model with m=100#####################################
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/logistic-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==3){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==3){
    Anp=rbind(Anp,save_res$Anp)
    se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==3)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,0,a2),length(M)),ncol=3,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,0,a2)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==3)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,0,a2),length(M_np)),ncol=3,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,0,a2)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,0,a2),nrep),ncol=3,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,0,a2)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(10,13,16),5:8]=c(abias,asd,ase,acp)
results[c(11,14,17),5:8]=c(abias_or,asd_or,ase_or,acp_or)
results[c(12,15,18),5:8]=c(abias_np,asd_np,ase_np,acp_np)
#############################overdispersed poisson model with m=50#####################################
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/nbinom-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==3){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==3){
    Anp=rbind(Anp,save_res$Anp)
    se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==3)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,0,a2),length(M)),ncol=3,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,0,a2)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==3)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,0,a2),length(M_np)),ncol=3,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,0,a2)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,0,a2),nrep),ncol=3,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,0,a2)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(19,22,25),1:4]=c(abias,asd,ase,acp)
results[c(20,23,26),1:4]=c(abias_or,asd_or,ase_or,acp_or)
results[c(21,24,27),1:4]=c(abias_np,asd_np,ase_np,acp_np)

#############################overdispersed poisson model with m=100#####################################
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/nbinom-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==3){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==3){
    Anp=rbind(Anp,save_res$Anp)
    se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==3)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,0,a2),length(M)),ncol=3,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,0,a2)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==3)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,0,a2),length(M_np)),ncol=3,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,0,a2)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,0,a2),nrep),ncol=3,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,0,a2)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(19,22,25),5:8]=c(abias,asd,ase,acp)
results[c(20,23,26),5:8]=c(abias_or,asd_or,ase_or,acp_or)
results[c(21,24,27),5:8]=c(abias_np,asd_np,ase_np,acp_np)

#############################5 groups with m=50#####################################
a1=-2
a2=-1
a3=1
a4=2
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/5groups-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==5){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==5){
    Anp=rbind(Anp,save_res$Anp)
    se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==5)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,a2,0,a3,a4),length(M)),ncol=5,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,a2,0,a3,a4)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==5)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,a2,0,a3,a4),length(M_np)),ncol=5,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,a2,0,a3,a4)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,a2,0,a3,a4),nrep),ncol=5,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,a2,0,a3,a4)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(28,30,32,34,36),1:4]=c(abias,asd,ase,acp)
results[c(29,31,33,35,37),1:4]=c(abias_or,asd_or,ase_or,acp_or)
#############################5 groups with m=100#####################################
ngroup=ngroup_np=Aest=se.a=cp.a=Aor=se.a_or=Anp=se.a_np=NULL
for(i in 1:nrep){
  res= paste('Results/5groups-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  ngroup=c(ngroup,save_res$ngroup)
  ngroup_np=c(ngroup_np,save_res$ngroup_np)
  if(save_res$ngroup==5){
    Aest=rbind(Aest,save_res$Aest)
    se.a=rbind(se.a,save_res$se.a)
  }
  if(save_res$ngroup_np==5){
    Anp=rbind(Anp,save_res$Anp)
    se.a_np=rbind(se.a_np,save_res$se.a_np)
  }
  Aor=rbind(Aor,save_res$Aor)
  se.a_or=rbind(se.a_or,save_res$se.a_or)
}

#SCAD
M=which(ngroup==5)
cp.a=ifelse(abs(Aest-matrix(rep(c(a1,a2,0,a3,a4),length(M)),ncol=5,byrow = TRUE))<1.96*se.a,1,0)
amean=apply(Aest,2,mean)
abias=abs(c(a1,a2,0,a3,a4)-amean)
asd=apply(Aest,2,sd)
ase=apply(se.a,2,mean)
acp=apply(cp.a,2,mean)
#######latent class#######
M_np=which(ngroup_np==5)
cp.a_np=ifelse(abs(Anp-matrix(rep(c(a1,a2,0,a3,a4),length(M_np)),ncol=5,byrow = TRUE))<1.96*se.a_np,1,0)
amean_np=apply(Anp,2,mean)
abias_np=abs(c(a1,a2,0,a3,a4)-amean_np)
asd_np=apply(Anp,2,sd)
ase_np=apply(se.a_np,2,mean)
acp_np=apply(cp.a_np,2,mean)
###############oracle######################
cp.a_or=ifelse(abs(Aor-matrix(rep(c(a1,a2,0,a3,a4),nrep),ncol=5,byrow = TRUE))<1.96*se.a_or,1,0)
amean_or=apply(Aor,2,mean)
abias_or=abs(c(a1,a2,0,a3,a4)-amean_or)
asd_or=apply(Aor,2,sd)
ase_or=apply(se.a_or,2,mean)
acp_or=apply(cp.a_or,2,mean)

results[c(28,30,32,34,36),5:8]=c(abias,asd,ase,acp)
results[c(29,31,33,35,37),5:8]=c(abias_or,asd_or,ase_or,acp_or)

Model = c(rep("Poisson with 3 groups", 9), rep("Logistic with 3 groups", 9),
          rep("Overdispersed Poisson with 3 groups", 9),rep("Poisson with 5 groups", 10))
Method = c(rep(c("SCAD", "Oracle", "LC"), 9),rep(c("SCAD", "Oracle"), 5))
Parameter =c(rep(rep(c("\u03B4\u03041", "\u03B4\u03042", "\u03B4\u03043"), each = 3),3),
             rep(c("\u03B4\u03041", "\u03B4\u03042", "\u03B4\u03043","\u03B4\u03044","\u03B4\u03045"), each = 2))
colnames(results) <- c("Bias_50", "SD_50", "SE_50", "CP_50","Bias_100", "SD_100", "SE_100", "CP_100")
Table2 <- data.frame(Model=Model,Method=Method,Parameter=Parameter,results)
sink("Results/Table2.csv",append=FALSE,split=TRUE)
cat("\n### Table 2 in Simulation Results ###\n")
print(Table2)
sink()
