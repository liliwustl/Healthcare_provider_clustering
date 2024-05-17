library(MASS)
library(ggplot2)
library(grid)
library (gridExtra)
library(npmlreg)
T=nrep=100
#############################poisson model with m=50#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=NULL
for(i in 1:nrep){
  res= paste('Results/poisson-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}
#boxplot of aMSE
data1=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data2=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data3=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data4=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data5=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a1=rbind(data1,data2,data3,data4,data5)
names(data_a1)=c("Methods","MSE")

f1=ggplot(data_a1,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Poisson Model with 3 groups,m=50",x="Methods",y="MSE")

#############################poisson model with m=100#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=NULL
for(i in 1:nrep){
  res= paste('Results/poisson-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}


data6=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data7=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data8=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data9=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data10=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a2=rbind(data6,data7,data8,data9,data10)
names(data_a2)=c("Methods","MSE")

f2=ggplot(data_a2,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Poisson Model with 3 groups,m=100",x="Methods",y="MSE ")


#############################logistic model with m=50#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=NULL
for(i in 1:nrep){
  res= paste('Results/logistic-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}

data11=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data12=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data13=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data14=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data15=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a3=rbind(data11,data12,data13,data14,data15)
names(data_a3)=c("Methods","MSE")

f3=ggplot(data_a3,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Logistic Model with 3 groups,m=50",x="Methods",y="MSE")


#############################logistic model with m=100#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=NULL
for(i in 1:nrep){
  res= paste('Results/logistic-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}

data16=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data17=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data18=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data19=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data20=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a4=rbind(data16,data17,data18,data19,data20)
names(data_a4)=c("Methods","MSE")

f4=ggplot(data_a4,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Logistic Model with 3 groups,m=100",x="Methods",y="MSE")



#############################overdispersed poisson model with m=50#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=aMSE_rqs=aMSE_fqs=NULL
betaest=se.beta=betaran=se.beta_ran=betafix=se.beta_fix=betaor=se.beta_or=betanp=se.beta_np=betarqs=se.beta_rqs=betafqs=se.beta_fqs=NULL
for(i in 1:nrep){
  res= paste('Results/nbinom-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_rqs=c(aMSE_rqs,save_res$aMSE_rqs)
  aMSE_fqs=c(aMSE_fqs,save_res$aMSE_fqs)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}


#boxplot of aMSE
data21=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data22=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data23=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data24=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data25=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a5=rbind(data21,data22,data23,data24,data25)
names(data_a5)=c("Methods","MSE")

f5=ggplot(data_a5,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Overdispersed Poisson Model with 3 groups,m=50",x="Methods",y="MSE")



#############################overdispersed poisson model with m=100#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=aMSE_rqs=aMSE_fqs=NULL
betaest=se.beta=betaran=se.beta_ran=betafix=se.beta_fix=betaor=se.beta_or=betanp=se.beta_np=betarqs=se.beta_rqs=betafqs=se.beta_fqs=NULL
for(i in 1:nrep){
  res= paste('Results/nbinom-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_rqs=c(aMSE_rqs,save_res$aMSE_rqs)
  aMSE_fqs=c(aMSE_fqs,save_res$aMSE_fqs)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}
data26=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data27=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data28=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data29=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data30=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a6=rbind(data26,data27,data28,data29,data30)
names(data_a6)=c("Methods","MSE")

f6=ggplot(data_a6,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Overdispersed Poisson Model with 3 groups,m=100",x="Methods",y="MSE")

#############################5 groups with m=50#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=NULL
betaest=se.beta=betaran=se.beta_ran=betafix=se.beta_fix=betaor=se.beta_or=betanp=se.beta_np=NULL
for(i in 1:nrep){
  res= paste('Results/5groups-50/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}

#boxplot of aMSE
data31=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data32=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data33=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data34=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data35=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a7=rbind(data31,data32,data33,data34,data35)
names(data_a7)=c("Methods","MSE")

f7=ggplot(data_a7,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Poisson Model with 5 groups,m=50",x="Methods",y="MSE")

#############################5 groups with m=100#####################################
aMSE=aMSE_or=aMSE_fix=aMSE_ran=aMSE_np=NULL
betaest=se.beta=betaran=se.beta_ran=betafix=se.beta_fix=betaor=se.beta_or=betanp=se.beta_np=NULL
for(i in 1:nrep){
  res= paste('Results/5groups-100/intermediate_results',i,'.Rdata',sep="")
  load(res)
  aMSE=c(aMSE,save_res$aMSE)
  aMSE_ran=c(aMSE_ran,save_res$aMSE_ran)
  aMSE_fix=c(aMSE_fix,save_res$aMSE_fix)
  aMSE_np=c(aMSE_np,save_res$aMSE_np)
  aMSE_or=c(aMSE_or,save_res$aMSE_or)
}

data36=data.frame(x=rep(as.factor("SCAD"),100),y=aMSE)
data37=data.frame(x=rep(as.factor("FE"),100),y=aMSE_fix)
data38=data.frame(x=rep(as.factor("RE"),100),y=aMSE_ran)
data39=data.frame(x=rep(as.factor("ORACLE"),100),y=aMSE_or)
data40=data.frame(x=rep(as.factor("LC"),100),y=aMSE_np)
data_a8=rbind(data36,data37,data38,data39,data40)
names(data_a8)=c("Methods","MSE")

f8=ggplot(data_a8,aes(x=Methods,y=MSE))+geom_boxplot(outlier.size=0.5, fatten=1)+
  theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=15),axis.title.x=element_text(size=15),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.border=element_rect(fill="transparent",colour='black'),
        panel.grid.major=element_line(colour='lightgrey',linetype="dashed"),
        plot.title=element_text(hjust=0.5,size=15),plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(title="Poisson Model with 5 groups,m=100",x="Methods",y="MSE")


figure=grid.arrange(f1,f3,f5,f7,f2,f4,f6,f8,ncol=4,nrow=2)
ggsave("Results/figure1.pdf",figure,width = 60, height = 30, units = "cm")
