#####################################################
# 
#         Sim 1.1
#
#####################################################

N1=N2=800
reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

SE11=SE21=SE31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)

for (rep in 1:50){
  Trait10=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian3_hard_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian2_hard_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore1_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore2_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore3_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore4_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard.csv')[((rep-1)*N2+1):(rep*N2),])
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}


cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))
cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))
cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias12[1:reps,]),colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias12[1:reps,]),colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse12[1:reps,]),colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))

# SE
for (rep in 1:50){
  Trait10=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian3_hard_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian2_hard_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore1_hard_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore2_hard_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore3_hard_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore4_hard_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}

round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)

##################
#   clustering
##################
reps=50
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[400]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[1200])&(change.sco>sort(change.sco)[400])),]
  grp3=change.sco2[which((change.sco>sort(change.sco)[1200])),]
  
  Trait10=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian3_hard_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian2_hard_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore1_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore2_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore3_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore4_hard_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  Trait10=rbind(Trait11,Trait12)
  Trait20=rbind(Trait21,Trait22)
  Trait30=rbind(Trait31,Trait32)
  
  Trait10=cbind(Trait10,Trait10[,2]-Trait10[,1])
  Trait20=cbind(Trait20,Trait20[,2]-Trait20[,1])
  Trait30=cbind(Trait30,Trait30[,2]-Trait30[,1])
  
  grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[400])]
  grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[1200])&((Trait10[,3])>sort(change.sco)[400]))]
  grp13=(1:N)[which((Trait10[,3])>sort(change.sco)[1200])]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[400])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[1200])&((Trait20[,3])>sort(change.sco)[400]))]
  grp23=(1:N)[which(((Trait20[,3])>sort(change.sco)[1200]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[400])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[1200])&((Trait30[,3])>sort(change.sco)[400]))]
  grp33=(1:N)[which(((Trait30[,3])>sort(change.sco)[1200]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


# plots 

library(ggplot2)
visit.cohot=c(rep("C1 V1",3),rep("C1 V2",3),rep("C1 CS",3),rep("C2 V1",3),rep("C2 V2",3),rep("C2 CS",3))
bias=c(0.001,	-0.010,	0.015,	-0.019,	-0.031,	-0.016, -0.020,	-0.021,	-0.031, 0.006,	-0.009,	0.006,	-0.020,	-0.048,	-0.021, -0.027,	-0.039,	-0.027)
Method=rep(c("Concurrent","Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$bias=as.numeric(data$bias)
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) + geom_hline(yintercept = 0) +
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")
p1

rmse=c(0.424,	0.425,	0.435,	0.421,	0.423,	0.430 ,0.544,	0.544,	0.562, 0.430,	0.431	,0.438,	0.422	,0.425	,0.431, 0.548	,0.550,	0.566)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$rmse=as.numeric(data$rmse)
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.336,	0.337,	0.344,	0.333,	0.334,	0.340,0.433,	0.433,	0.447,0.339,	0.340,	0.345,	0.334	,0.336,	0.341,0.435,	0.437,	0.449)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$absbias=as.numeric(data$absbias)
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3


SEs=c(0.413,	0.405,	0.414,	0.414,	0.390,	0.406,0.586,	0.564,	0.582,0.413,	0.406,	0.412,	0.418,	0.395,0.403,0.589,	0.568,	0.578)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$SEs=as.numeric(data$SEs)
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4


library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")


# item parameters
meanbias1=meanbias2=meanbias3=matrix(0,reps,2)
bias1=bias2=bias3=matrix(0,reps,2)
rmse1=rmse2=rmse3=matrix(0,reps,2)

for (rep in 1:50){
  a1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(1:6,10:12),3] # no constraint
  a2=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian3_hard_",rep,".out"))$parameters$stdyx.standardized[c(1:6,10:12),3]
  a3=c(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore1_hard_",rep,".out"))$parameters$stdyx.standardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore3_hard_",rep,".out"))$parameters$stdyx.standardized[c(4:6),3])
  
  d1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3] # no constraint
  d2=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian3_hard_",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3]
  d3=c(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore1_hard_",rep,".out"))$parameters$unstandardized[c(8:25),3],
       readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore3_hard_",rep,".out"))$parameters$unstandardized[c(17:25),3])
  
  a=c(Amat1[1:6],Amat2[4:6])
  b=matrix(t(grorb),27,1)
  
  meanbias1[rep,1]=mean(abs(a1-a))
  meanbias2[rep,1]=mean(abs(a2-a))
  meanbias3[rep,1]=mean(abs(a3-a))
  
  meanbias1[rep,2]=colMeans(abs(d1-b))
  meanbias2[rep,2]=colMeans(abs(d2-b))
  meanbias3[rep,2]=colMeans(abs(d3-b))
  
  bias1[rep,1]=mean(a1-a)
  bias2[rep,1]=mean(a2-a)
  bias3[rep,1]=mean(a3-a)
  
  bias1[rep,2]=mean(d1-b)
  bias2[rep,2]=mean(d2-b)
  bias3[rep,2]=mean(d3-b)
  
  rmse1[rep,1]=sqrt(mean((a1-a)^2))
  rmse2[rep,1]=sqrt(mean((a2-a)^2))
  rmse3[rep,1]=sqrt(mean((a3-a)^2))
  
  rmse1[rep,2]=sqrt(mean((d1-b)^2))
  rmse2[rep,2]=sqrt(mean((d2-b)^2))
  rmse3[rep,2]=sqrt(mean((d3-b)^2))
}
c(colMeans(meanbias1[1:reps,]),colMeans(meanbias2[1:reps,]),colMeans(meanbias3[1:reps,]))

round(c(colMeans(bias1[1:reps,]),colMeans(bias2[1:reps,]),colMeans(bias3[1:reps,])),3)

round(c(colMeans(rmse1[1:reps,]),colMeans(rmse2[1:reps,]),colMeans(rmse3[1:reps,])),3)


#####################################################
# 
#         Sim 1.2
#
#####################################################

N1=N2=800
reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)
for (rep in 1:50){
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_concurrent_shorttest_hard_imp",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_bayesian3_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_bayesian2_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore1_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore2_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore3_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore4_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_imp.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_imp.csv')[((rep-1)*N2+1):(rep*N2),])
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}


plot(Trait10[,2],Trait[,3])
mean(abs(Trait10[,2]-Trait[,3]))
abline(0,1)
plot(c(Trait20[,2],Trait20[,4]),Trait[,3])
mean(abs(c(Trait20[,2],Trait20[,4])-Trait[,3]))
abline(0,1)


rep



round(cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])),3)
round(cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])),3)
round(cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])),3)


round(cbind(colMeans(meanbias12[11:reps,]),colMeans(meanbias22[11:reps,]),colMeans(meanbias32[11:reps,])),3)
round(cbind(colMeans(bias12[11:reps,]),colMeans(bias22[11:reps,]),colMeans(bias32[11:reps,])),3)
round(cbind(colMeans(rmse12[11:reps,]),colMeans(rmse22[11:reps,]),colMeans(rmse32[11:reps,])),3)

# SE
for (rep in 1:50){
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_concurrent_shorttest_hard_imp",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_bayesian3_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_bayesian2_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore1_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore2_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore3_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore4_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}

round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)

##################
#   clustering
##################
reps=50
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_imp.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_imp.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[400]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[1200])&(change.sco>sort(change.sco)[400])),]
  grp3=change.sco2[which((change.sco>sort(change.sco)[1200])),]
  
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_concurrent_shorttest_hard_imp",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_bayesian3_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_bayesian2_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore1_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore2_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore3_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_shorttest_ignore4_hard_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  Trait10=rbind(Trait11,Trait12)
  Trait20=rbind(Trait21,Trait22)
  Trait30=rbind(Trait31,Trait32)
  
  Trait10=cbind(Trait10,Trait10[,2]-Trait10[,1])
  Trait20=cbind(Trait20,Trait20[,2]-Trait20[,1])
  Trait30=cbind(Trait30,Trait30[,2]-Trait30[,1])
  
  grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[400])]
  grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[1200])&((Trait10[,3])>sort(change.sco)[400]))]
  grp13=(1:N)[which((Trait10[,3])>sort(change.sco)[1200])]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[400])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[1200])&((Trait20[,3])>sort(change.sco)[400]))]
  grp23=(1:N)[which(((Trait20[,3])>sort(change.sco)[1200]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[400])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[1200])&((Trait30[,3])>sort(change.sco)[400]))]
  grp33=(1:N)[which(((Trait30[,3])>sort(change.sco)[1200]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


# plots 

library(ggplot2)
visit.cohot=c(rep("C1 V1",3),rep("C1 V2",3),rep("C1 CS",3),rep("C2 V1",3),rep("C2 V2",3),rep("C2 CS",3))
bias=c(-0.006,	-0.012,	0.008,	-0.035,	-0.036,	-0.022,-0.03,	-0.023,	-0.030,0.009,	-0.015,	0.008,	-0.012,	-0.047,	-0.014,-0.021,	-0.032,	-0.006)
Method=rep(c("Concurrent","Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p <-ggplot(data, aes(x=data$visit.cohot, y=data$bias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")
p

rmse=c(0.380,	0.380,	0.435,	0.420,	0.415,	0.456,0.407,	0.404,	0.548,0.380,	0.378,	0.453,	0.402,	0.400,	0.464,0.306,	0.307,	0.464)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p <-ggplot(data, aes(x=data$visit.cohot, y=data$rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE")
p


absbias=c(0.299,	0.299,	0.343,	0.331,	0.326,	0.359,0.324,	0.321,	0.435,0.299,	0.297,	0.356,	0.317,	0.314,	0.364,0.244,	0.244,	0.429)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")
p


SEs=c(0.369,	0.361,	0.416,	0.420,	0.386,	0.432,0.560,	0.529,	0.602,0.374,	0.360,	0.429,	0.399,	0.375,	0.434,0.547,	0.521,	0.611)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")
p



library(ggplot2)
visit.cohot=c(rep("C1 V1",3),rep("C1 V2",3),rep("C1 CS",3),rep("C2 V1",3),rep("C2 V2",3),rep("C2 CS",3))
bias=c(-0.006,	-0.012,	0.008,	-0.035,	-0.036,	-0.022,-0.03,	-0.023,	-0.030,0.009,	-0.015,	0.008,	-0.012,	-0.047,	-0.014,-0.021,	-0.032,	-0.006)
Method=rep(c("Concurrent","Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$bias=as.numeric(data$bias)
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) + geom_hline(yintercept = 0) +
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")
p1

rmse=c(0.380,	0.380,	0.435,	0.420,	0.415,	0.456,0.407,	0.404,	0.548,0.380,	0.378,	0.453,	0.402,	0.400,	0.464,0.306,	0.307,	0.464)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$rmse=as.numeric(data$rmse)
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.299,	0.299,	0.343,	0.331,	0.326,	0.359,0.324,	0.321,	0.435,0.299,	0.297,	0.356,	0.317,	0.314,	0.364,0.244,	0.244,	0.429)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$absbias=as.numeric(data$absbias)
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3


SEs=c(0.369,	0.361,	0.416,	0.420,	0.386,	0.432,0.560,	0.529,	0.602,0.374,	0.360,	0.429,	0.399,	0.375,	0.434,0.547,	0.521,	0.611)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$SEs=as.numeric(data$SEs)
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4


library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")

# item parameters
meanbias1=meanbias2=meanbias3=matrix(0,reps,2)
bias1=bias2=bias3=matrix(0,reps,2)
rmse1=rmse2=rmse3=matrix(0,reps,2)

for (rep in 1:50){
  a1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(1:6,10:12),3] # no constraint
  a2=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian3_hard_",rep,".out"))$parameters$unstandardized[c(1:6,10:12),3]
  a3=c(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore1_hard_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore3_hard_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  
  d1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3] # no constraint
  d2=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Bayesian/sim2_shorttest_bayesian3_hard_",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3]
  d3=c(readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore1_hard_",rep,".out"))$parameters$unstandardized[c(8:25),3],
       readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Ignore/sim2_shorttest_ignore3_hard_",rep,".out"))$parameters$unstandardized[c(17:25),3])
  
  a=c(Amat1[1:6],Amat2[4:6])
  b=matrix(t(grorb),27,1)
  
  meanbias1[rep,1]=mean(abs(a1-a))
  meanbias2[rep,1]=mean(abs(a2-a))
  meanbias3[rep,1]=mean(abs(a3-a))
  
  meanbias1[rep,2]=colMeans(abs(d1-b))
  meanbias2[rep,2]=colMeans(abs(d2-b))
  meanbias3[rep,2]=colMeans(abs(d3-b))
  
  bias1[rep,1]=mean(a1-a)
  bias2[rep,1]=mean(a2-a)
  bias3[rep,1]=mean(a3-a)
  
  bias1[rep,2]=mean(d1-b)
  bias2[rep,2]=mean(d2-b)
  bias3[rep,2]=mean(d3-b)
  
  rmse1[rep,1]=sqrt(mean((a1-a)^2))
  rmse2[rep,1]=sqrt(mean((a2-a)^2))
  rmse3[rep,1]=sqrt(mean((a3-a)^2))
  
  rmse1[rep,2]=sqrt(mean((d1-b)^2))
  rmse2[rep,2]=sqrt(mean((d2-b)^2))
  rmse3[rep,2]=sqrt(mean((d3-b)^2))
}
c(colMeans(meanbias1[1:reps,]),colMeans(meanbias2[1:reps,]),colMeans(meanbias3[1:reps,]))

round(c(colMeans(bias1[1:reps,]),colMeans(bias2[1:reps,]),colMeans(bias3[1:reps,])),3)

round(c(colMeans(rmse1[1:reps,]),colMeans(rmse2[1:reps,]),colMeans(rmse3[1:reps,])),3)


############################
# 
#         Sim1.3
#
############################

psr1=numeric(20)

for (rep in 1:20){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  X=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_concurrent_adnilan",rep,".out"))
  psr1[rep]=get_tech8(X)$psr[1000,2]
}


N1=N2=800
reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

for (rep in 1:50){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_concurrent_adnilan",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian3_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian2_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore1_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore2_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore3_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore4_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  # #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan.csv')[((rep-1)*N2+1):(rep*N2),])
  #meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}


cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))

cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))

cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))

# SE
for (rep in 1:50){
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian3_adnilan_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian2_adnilan_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore1_adnilan_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore2_adnilan_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore3_adnilan_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore4_adnilan_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  #Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  #SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  #SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}

round(cbind(colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)

##################
#   clustering
##################
reps=50
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[400]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[1200])&(change.sco>sort(change.sco)[400])),]
  grp3=change.sco2[which((change.sco>sort(change.sco)[1200])),]
  
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian3_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian2_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore1_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore2_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore3_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore4_adnilan_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #Trait10=rbind(Trait11,Trait12)
  Trait20=rbind(Trait21,Trait22)
  Trait30=rbind(Trait31,Trait32)
  
  #Trait10=cbind(Trait10,Trait10[,2]-Trait10[,1])
  Trait20=cbind(Trait20,Trait20[,2]-Trait20[,1])
  Trait30=cbind(Trait30,Trait30[,2]-Trait30[,1])
  
  #grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[400])]
  #grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[1200])&((Trait10[,3])>sort(change.sco)[400]))]
  #grp13=(1:N)[which((Trait10[,3])>sort(change.sco)[1200])]
  
  #a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[400])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[1200])&((Trait20[,3])>sort(change.sco)[400]))]
  grp23=(1:N)[which(((Trait20[,3])>sort(change.sco)[1200]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[400])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[1200])&((Trait30[,3])>sort(change.sco)[400]))]
  grp33=(1:N)[which(((Trait30[,3])>sort(change.sco)[1200]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
#a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


# plots 

library(ggplot2)
visit.cohot=c(rep("C1 V1",2),rep("C1 V2",2),rep("C1 CS",2),rep("C2 V1",2),rep("C2 V2",2),rep("C2 CS",2))
bias=c(-0.006,	-0.001,	-0.008,	0.012,-0.002,	0.012,-0.030,	-0.002,	-0.040,	-0.019,-0.010,	-0.016)
Method=rep(c("Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$bias=as.numeric(data$bias)
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")+ geom_hline(yintercept = 0)
p1


rmse=c(0.491,	0.493,	0.497,	0.499,0.558,	0.565,0.522,	0.522,	0.522,	0.522,0.565,	0.570)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$rmse=as.numeric(data$rmse)
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.390,	0.392,	0.394,	0.396,0.442,	0.448,0.415,	0.416,	0.416,	0.417,0.448,	0.452)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$absbias=as.numeric(data$absbias)
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3


SEs=c(0.480,	0.483,	0.477,	0.476,0.678,	0.680,0.479,	0.506,	0.485,	0.504,0.682,	0.716)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$SEs=as.numeric(data$SEs)
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4

library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")


# item parameters
meanbias1=meanbias2=meanbias3=matrix(0,reps,2)
bias1=bias2=bias3=matrix(0,reps,2)
rmse1=rmse2=rmse3=matrix(0,reps,2)

set.seed(1)
Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
J=9
set.seed(1)
grb1<-runif(J,-3,-1)
grb2<-runif(J,-1,1)
grb3<-runif(J,1,3)


grb<-matrix(c(grb1,grb2,grb3),nrow=J)
grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
grorb
round(grorb,3)
for (rep in 1:10){
  #a1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(1:6,10:12),3] # no constraint
  a2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  a3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  
  #d1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3] # no constraint
  d2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(61:78),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(70:78),3])
  d3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(8:25),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(17:25),3])
  
  a=c(Amat1[1:6],Amat2[4:6])
  b=matrix(t(grorb),27,1)
  
  # meanbias1[rep,1]=mean(abs(a1-a))
  meanbias2[rep,1]=mean(abs(a2-a))
  meanbias3[rep,1]=mean(abs(a3-a))
  
  # meanbias1[rep,2]=colMeans(abs(d1-b))
  meanbias2[rep,2]=colMeans(abs(d2-b))
  meanbias3[rep,2]=colMeans(abs(d3-b))
  
  #bias1[rep,1]=mean(a1-a)
  bias2[rep,1]=mean(a2-a)
  bias3[rep,1]=mean(a3-a)
  
  #bias1[rep,2]=mean(d1-b)
  bias2[rep,2]=mean(d2-b)
  bias3[rep,2]=mean(d3-b)
  
  #rmse1[rep,1]=sqrt(mean((a1-a)^2))
  rmse2[rep,1]=sqrt(mean((a2-a)^2))
  rmse3[rep,1]=sqrt(mean((a3-a)^2))
  
  #rmse1[rep,2]=sqrt(mean((d1-b)^2))
  rmse2[rep,2]=sqrt(mean((d2-b)^2))
  rmse3[rep,2]=sqrt(mean((d3-b)^2))
}
round(c(colMeans(meanbias2[1:reps,]),colMeans(meanbias3[1:reps,])),3)

round(c(colMeans(bias2[1:reps,]),colMeans(bias3[1:reps,])),3)

round(c(colMeans(rmse2[1:reps,]),colMeans(rmse3[1:reps,])),3)


#####################################################
# 
#         Sim1.4
#
#####################################################
N1=N2=800
reps=50

#psr
psr1=numeric(reps)

for (rep in 1:reps){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  X=readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_concurrent_adnilan_imp",rep,".out"))
  psr1[rep]=get_tech8(X)$psr[1000,2]
}

meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)
for (rep in 1:50){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore4_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  # #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_imp.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_imp.csv')[((rep-1)*N2+1):(rep*N2),])
  #meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}


cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))

cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))

cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))

# SE
for (rep in 1:50){
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore4_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  #Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  #SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  #SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}

round(cbind(colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)

##################
#   clustering
##################
reps=50
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_imp.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_imp.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[400]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[1200])&(change.sco>sort(change.sco)[400])),]
  grp3=change.sco2[which((change.sco>sort(change.sco)[1200])),]
  
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore4_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #Trait10=rbind(Trait11,Trait12)
  Trait20=rbind(Trait21,Trait22)
  Trait30=rbind(Trait31,Trait32)
  
  #Trait10=cbind(Trait10,Trait10[,2]-Trait10[,1])
  Trait20=cbind(Trait20,Trait20[,2]-Trait20[,1])
  Trait30=cbind(Trait30,Trait30[,2]-Trait30[,1])
  
  #grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[400])]
  #grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[1200])&((Trait10[,3])>sort(change.sco)[400]))]
  #grp13=(1:N)[which((Trait10[,3])>sort(change.sco)[1200])]
  
  #a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[400])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[1200])&((Trait20[,3])>sort(change.sco)[400]))]
  grp23=(1:N)[which(((Trait20[,3])>sort(change.sco)[1200]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[400])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[1200])&((Trait30[,3])>sort(change.sco)[400]))]
  grp33=(1:N)[which(((Trait30[,3])>sort(change.sco)[1200]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
#a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


# plots 

library(ggplot2)
visit.cohot=c(rep("C1 V1",2),rep("C1 V2",2),rep("C1 CS",2),rep("C2 V1",2),rep("C2 V2",2),rep("C2 CS",2))
bias=c(-0.008,	-0.005,	-0.008,	0.010,0.0001,	0.014,-0.029,	-0.004,	-0.031,	-0.018,-0.002,	-0.014)
Method=rep(c("Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$bias=as.numeric(data$bias)
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")+ geom_hline(yintercept = 0)
p1

rmse=c(0.452,	0.492,	0.502,	0.526,0.407,	0.525,0.482,	0.538,	0.510,	0.550,0.306,	0.519)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$rmse=as.numeric(data$rmse)
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.360,	0.391,	0.398,	0.417,0.324,	0.417,0.384,	0.427,	0.406,	0.438,0.244,	0.412)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$absbias=as.numeric(data$absbias)
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3

SEs=c(0.461,	0.523,	0.494,	0.531,0.674,	0.696,0.461,	0.523,	0.494,	0.531,0.676,	0.747)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$SEs=as.numeric(data$SEs)
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4


library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")

# item parameters
meanbias1=meanbias2=meanbias3=matrix(0,reps,2)
bias1=bias2=bias3=matrix(0,reps,2)
rmse1=rmse2=rmse3=matrix(0,reps,2)

set.seed(1)
Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
J=9
set.seed(1)
grb1<-runif(J,-3,-1)
grb2<-runif(J,-1,1)
grb3<-runif(J,1,3)


grb<-matrix(c(grb1,grb2,grb3),nrow=J)
grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
grorb
round(grorb,3)
for (rep in 1:10){
  #a1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(1:6,10:12),3] # no constraint
  a2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  a3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  
  #d1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3] # no constraint
  d2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(61:78),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(70:78),3])
  d3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(8:25),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(17:25),3])
  
  a=c(Amat1[1:6],Amat2[4:6])
  b=matrix(t(grorb),27,1)
  
  # meanbias1[rep,1]=mean(abs(a1-a))
  meanbias2[rep,1]=mean(abs(a2-a))
  meanbias3[rep,1]=mean(abs(a3-a))
  
  # meanbias1[rep,2]=colMeans(abs(d1-b))
  meanbias2[rep,2]=colMeans(abs(d2-b))
  meanbias3[rep,2]=colMeans(abs(d3-b))
  
  #bias1[rep,1]=mean(a1-a)
  bias2[rep,1]=mean(a2-a)
  bias3[rep,1]=mean(a3-a)
  
  #bias1[rep,2]=mean(d1-b)
  bias2[rep,2]=mean(d2-b)
  bias3[rep,2]=mean(d3-b)
  
  #rmse1[rep,1]=sqrt(mean((a1-a)^2))
  rmse2[rep,1]=sqrt(mean((a2-a)^2))
  rmse3[rep,1]=sqrt(mean((a3-a)^2))
  
  #rmse1[rep,2]=sqrt(mean((d1-b)^2))
  rmse2[rep,2]=sqrt(mean((d2-b)^2))
  rmse3[rep,2]=sqrt(mean((d3-b)^2))
}
round(c(colMeans(meanbias2[1:reps,]),colMeans(meanbias3[1:reps,])),3)

round(c(colMeans(bias2[1:reps,]),colMeans(bias3[1:reps,])),3)

round(c(colMeans(rmse2[1:reps,]),colMeans(rmse3[1:reps,])),3)


#####################################################
# 
#         Sim 2.1
#
#####################################################

N1=N2=800
reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)


SE11=SE21=SE31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)
# concurrent not converge: 9, 11,17
# lowcor problematic: 
for (rep in c(1:50)){
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_concurrent_shorttest_hard_clus_lowcor",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian3_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian2_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore1_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore2_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore3_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore4_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}

meanbias11=meanbias11[-c(7, 20),]
meanbias21=meanbias21[-c(7, 20),]
meanbias31=meanbias31[-c(7, 20),]

bias11=bias11[-c(7, 20),]
bias21=bias21[-c(7, 20),]
bias31=bias31[-c(7, 20),]

rmse11=rmse11[-c(7, 20),]
rmse21=rmse21[-c(7, 20),]
rmse31=rmse31[-c(7, 20),]


meanbias12=meanbias12[-c(7, 20),]
meanbias22=meanbias22[-c(7, 20),]
meanbias32=meanbias32[-c(7, 20),]

bias12=bias12[-c(7, 20),]
bias22=bias22[-c(7, 20),]
bias32=bias32[-c(7, 20),]

rmse12=rmse12[-c(7, 20),]
rmse22=rmse22[-c(7, 20),]
rmse32=rmse32[-c(7, 20),]

reps=48
round(cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])),3)
round(cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])),3)
round(cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])),3)


round(cbind(colMeans(meanbias12[11:reps,]),colMeans(meanbias22[11:reps,]),colMeans(meanbias32[11:reps,])),3)
round(cbind(colMeans(bias12[11:reps,]),colMeans(bias22[11:reps,]),colMeans(bias32[11:reps,])),3)
round(cbind(colMeans(rmse12[11:reps,]),colMeans(rmse22[11:reps,]),colMeans(rmse32[11:reps,])),3)

# SE
for (rep in c(1:6,8:19,21:50)){
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_concurrent_shorttest_hard_clus_lowcor",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian3_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian2_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore1_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore2_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore3_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore4_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}

SE11=SE11[-c(7, 20),]
SE21=SE21[-c(7, 20),]
SE31=SE31[-c(7, 20),]
SE12=SE12[-c(7, 20),]
SE22=SE22[-c(7, 20),]
SE32=SE32[-c(7, 20),]

round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)

##################
#   clustering
##################
a1=a2=a3=0
#for (rep in c(10,12:16,18:50)){
for (rep in c(1:6,8:19,21:50)){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,3]-Trait[,2]
  change.sco2=cbind(1:N,Trait[,3]-Trait[,2])
  grp1=change.sco2[which(change.sco<=(-0.75)),]
  grp2=change.sco2[which((change.sco<=0.75)&(change.sco>-0.75)),]
  grp3=change.sco2[which(change.sco>0.75),]
  
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian3_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian2_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore1_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore2_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore3_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore4_hard_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  
  clus11=clus21=clus31=clus12=clus22=clus32=rep(2,800)
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  km11 <- kmeans( Trait11[,2]-Trait11[,1], 3, nstart=5000)
  cluster11=km11$cluster
  m1=mean((Trait11[,2]-Trait11[,1])[which( cluster11==1)])
  m2=mean((Trait11[,2]-Trait11[,1])[which( cluster11==2)])
  m3=mean((Trait11[,2]-Trait11[,1])[which( cluster11==3)])
  clus11[which( cluster11==which.min(c(m1,m2,m3)))]=1
  clus11[which( cluster11==which.max(c(m1,m2,m3)))]=3
  
  km21 <- kmeans((Trait21[,2]-Trait21[,1]), 3, nstart=5000)
  cluster21=km21$cluster
  m1=mean((Trait21[,2]-Trait21[,1])[which( cluster21==1)])
  m2=mean((Trait21[,2]-Trait21[,1])[which( cluster21==2)])
  m3=mean((Trait21[,2]-Trait21[,1])[which( cluster21==3)])
  clus21[which( cluster21==which.min(c(m1,m2,m3)))]=1
  clus21[which( cluster21==which.max(c(m1,m2,m3)))]=3
  
  km31 <- kmeans( Trait31[,2]-Trait31[,1], 3, nstart=5000)
  cluster31=km31$cluster
  m1=mean((Trait31[,2]-Trait31[,1])[which( cluster31==1)])
  m2=mean((Trait31[,2]-Trait31[,1])[which( cluster31==2)])
  m3=mean((Trait31[,2]-Trait31[,1])[which( cluster31==3)])
  clus31[which( cluster31==which.min(c(m1,m2,m3)))]=1
  clus31[which( cluster31==which.max(c(m1,m2,m3)))]=3
  
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  km12 <- kmeans( Trait12[,2]-Trait12[,1], 3, nstart=5000)
  cluster12=km12$cluster
  m1=mean((Trait12[,2]-Trait12[,1])[which( cluster12==1)])
  m2=mean((Trait12[,2]-Trait12[,1])[which( cluster12==2)])
  m3=mean((Trait12[,2]-Trait12[,1])[which( cluster12==3)])
  clus12[which( cluster12==which.min(c(m1,m2,m3)))]=1
  clus12[which( cluster12==which.max(c(m1,m2,m3)))]=3
  
  km22 <- kmeans((Trait22[,2]-Trait22[,1]), 3, nstart=5000)
  cluster22=km22$cluster
  m1=mean((Trait22[,2]-Trait22[,1])[which( cluster22==1)])
  m2=mean((Trait22[,2]-Trait22[,1])[which( cluster22==2)])
  m3=mean((Trait22[,2]-Trait22[,1])[which( cluster22==3)])
  clus22[which( cluster22==which.min(c(m1,m2,m3)))]=1
  clus22[which( cluster22==which.max(c(m1,m2,m3)))]=3
  
  
  km32 <- kmeans( Trait32[,2]-Trait32[,1], 3, nstart=5000)
  cluster32=km32$cluster
  m1=mean((Trait32[,2]-Trait32[,1])[which( cluster32==1)])
  m2=mean((Trait32[,2]-Trait32[,1])[which( cluster32==2)])
  m3=mean((Trait32[,2]-Trait32[,1])[which( cluster32==3)])
  clus32[which( cluster32==which.min(c(m1,m2,m3)))]=1
  clus32[which( cluster32==which.max(c(m1,m2,m3)))]=3
  
  
  
  grp11=(1:N)[which(c(clus11,clus12)==1)]
  grp12=(1:N)[which(c(clus11,clus12)==2)]
  grp13=(1:N)[which(c(clus11,clus12)==3)]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which(c(clus21,clus22)==1)]
  grp22=(1:N)[which(c(clus21,clus22)==2)]
  grp23=(1:N)[which(c(clus21,clus22)==3)]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which(c(clus31,clus32)==1)]
  grp32=(1:N)[which(c(clus31,clus32)==2)]
  grp33=(1:N)[which(c(clus31,clus32)==3)]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
reps=48
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875

cbind(change.sco[1:800],Trait11[,2]-Trait11[,1],Trait21[,2]-Trait21[,1],Trait31[,2]-Trait31[,1])
mean(change.sco[1:800]-(Trait11[,2]-Trait11[,1]))
sqrt(mean((change.sco[1:800]-(Trait11[,2]-Trait11[,1]))^2))
mean(abs(change.sco[1:800]-(Trait11[,2]-Trait11[,1])))

# plots 

visit.cohot=c(rep("C1 V1",3),rep("C1 V2",3),rep("C1 CS",3),rep("C2 V1",3),rep("C2 V2",3),rep("C2 CS",3))
bias=c(-0.009,	-0.029,	-0.001,0.006,	-0.050,	-0.029,0.015,	-0.023,	-0.028,-0.016,	-0.032,	-0.016,	0.009,	-0.063,	-0.021,0.025,	-0.031,	-0.005)
Method=rep(c("Concurrent","Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) + geom_hline(yintercept = 0) +
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")
p1

rmse=c(0.383,	0.384,	0.435,	0.405,	0.410,	0.450,0.404,	0.406,	0.544,0.386,	0.388,	0.438,	0.410,	0.414,	0.450,0.406,	0.406,	0.536)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.303,	0.304,	0.344,	0.321,	0.325,	0.355,0.326,	0.329,	0.433,0.305,	0.307,	0.346,	0.324,	0.328,	0.355,0.327,	0.327,	0.426)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3


SEs=c(0.368,	0.364,	0.415,	0.397,	0.382,	0.428,0.542,	0.528,	0.598,0.365,	0.369,	0.414,	0.396,	0.388,	0.427,0.539,	0.536,	0.596)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4


library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")


#####################################################
# 
#         Sim 2.2
#
#####################################################


## 6/18 plot
rep=10

Trait10=readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_concurrent_shorttest_hard_clus_imp",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian3_hard_clus_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
              readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian2_hard_clus_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore1_hard_clus_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
              readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore2_hard_clus_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
              readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore3_hard_clus_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
              readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore4_hard_clus_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")])

#cohort1
Trait11=Trait10[1:800,] # no constraint
Trait21=Trait20[,1:2]
Trait31=Trait30[,c(1,3)]

#cohort2
Trait12=Trait10[801:1600,] # no constraint
Trait22=Trait20[,3:4]
Trait32=Trait30[,c(2,4)]
#Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
#          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus_imp.csv')[((rep-1)*N1+1):(rep*N1),],
            read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus_imp.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]

library(ggplot2)
conc_bl_bias=(cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))[,1]
sing_bl_bias=(cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))[,1]
bias_BL=as.data.frame(cbind(conc_bl_bias,sing_bl_bias))
p <- ggplot(bias_BL, aes(x = conc_bl_bias, y = sing_bl_bias)) +
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5)+ coord_fixed()+xlim(-2,2)+ylim(-2,2)

p



conc_fu_bias=(cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))[,2]
sing_fu_bias=(cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))[,2]
bias_FU=as.data.frame(cbind(conc_fu_bias,sing_fu_bias))
p2 <- ggplot(bias_FU, aes(x = conc_fu_bias, y = sing_fu_bias)) +
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5)+ coord_fixed()+xlim(-2,2)+ylim(-2,2)

p2


conc_change_bias=(cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))[,3]
sing_change_bias=(cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))[,3]
bias_change=as.data.frame(cbind(conc_change_bias,sing_change_bias))
p3 <- ggplot(bias_change, aes(x = conc_change_bias, y = sing_change_bias)) +
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5)+ coord_fixed()+xlim(-2,2)+ylim(-2,2)

p3


#6/20 plots
#cohort1
Trait11=Trait10[1:800,] # no constraint
Trait21=Trait20[,1:2]
Trait31=Trait30[,c(1,3)]

#cohort2
Trait12=Trait10[801:1600,] # no constraint
Trait22=Trait20[,3:4]
Trait32=Trait30[,c(2,4)]
#Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
#          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus_imp.csv')[((rep-1)*N1+1):(rep*N1),],
            read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus_imp.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]

traitdata=as.data.frame(cbind(rep(1:1600,6),
             c(rbind(Trait11,Trait12)[,1],rbind(Trait11,Trait12)[,2],rbind(Trait31,Trait32)[,1],rbind(Trait31,Trait32)[,2],Trait[,2],Trait[,3]),
             c(rep("Concurrent",3200),rep("Single",3200),rep("True",3200)),
             c(rep(1,1600),rep(2,1600),rep(1,1600),rep(2,1600),rep(1,1600),rep(2,1600))))
colnames(traitdata)=c("RID","Trait","Method","Time")
traitdata$RID=factor(traitdata$RID,levels = unique(traitdata$RID))
traitdata$Trait=as.numeric(traitdata$Trait)
traitdata$Method=factor(traitdata$Method,levels = unique(traitdata$Method))
traitdata$Time=factor(traitdata$Time,levels = unique(traitdata$Time))
class(traitdata$RID)
class(traitdata$Trait)
class(traitdata$Time)
p4 <- ggplot(traitdata, aes(x = RID, y = Trait,color=Method,shape=Time))+
  geom_point() 

p4


traitdata2=as.data.frame(cbind(rep(1:100,6),
                              c(rbind(Trait11,Trait12)[1:100,1],rbind(Trait11,Trait12)[1:100,2],rbind(Trait31,Trait32)[1:100,1],rbind(Trait31,Trait32)[1:100,2],Trait[1:100,2],Trait[1:100,3]),
                              c(rep("Concurrent",200),rep("Single",200),rep("True",200)),
                              c(rep(1,100),rep(2,100),rep(1,100),rep(2,100),rep(1,100),rep(2,100))))
colnames(traitdata2)=c("RID","Trait","Method","Time")
traitdata2$RID=factor(traitdata2$RID,levels = unique(traitdata2$RID))
traitdata2$Trait=as.numeric(traitdata2$Trait)
traitdata2$Method=factor(traitdata2$Method,levels = unique(traitdata2$Method))
traitdata2$Time=factor(traitdata2$Time,levels = unique(traitdata2$Time))
class(traitdata2$RID)
class(traitdata2$Trait)
class(traitdata2$Time)
p5 <- ggplot(traitdata2, aes(x = RID, y = Trait,color=Method,shape=Time))+
  geom_point() 

p5

###########  Check  psr #########################

Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus_imp_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
            read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus_imp_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
cor(Trait[1:800,2],Trait[1:800,3])
cor(Trait[801:1600,2],Trait[801:1600,3])
N1=N2=800
#psr
psr1=numeric(reps)

for (rep in c(1:5,7:11,13:32,34:47,49:50)){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  X=readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_concurrent_shorttest_hard_clus_imp",rep,".out"))
  psr1[rep]=get_tech8(X)$psr[1000,2]
}

sum(psr1<1.2)

############# trait recovery ########################################

reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

SE11=SE21=SE31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)



plot(Trait10[,2],Trait[,3])
mean(abs(Trait10[,2]-Trait[,3]))
abline(0,1)

plot(Trait10[,1],Trait[,2])
mean(abs(Trait10[,1]-Trait[,2]))
abline(0,1)

plot(Trait10[,1],Trait[,2])
mean(abs((Trait10[,2]-Trait10[,1])-(Trait[,3]-Trait[,2])))
abline(0,1)

plot(c(Trait20[,2],Trait20[,4]),Trait[,3])
mean(abs(c(Trait20[,2],Trait20[,4])-Trait[,3]))
abline(0,1)

plot(c(Trait30[,3],Trait30[,4]),Trait[,3])
mean(abs(c(Trait30[,3],Trait30[,4])-Trait[,3]))
abline(0,1)

mean(abs(c(Trait30[,1],Trait30[,2])-Trait[,2]))
mean(abs((c(Trait30[,3],Trait30[,4])-c(Trait30[,1],Trait30[,2]))-(Trait[,3]-Trait[,2])))

hist(Trait[,3])
library(moments)
mean(Trait[,3])
var(Trait[,3])
skewness(Trait[,3])
kurtosis(Trait[,3])

#for (rep in c(1:5,7:11,13:32,34:47,49:50)){
for (rep in 1:50){
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_concurrent_shorttest_hard_clus_imp_lowcor",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian3_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian2_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore1_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore2_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore3_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore4_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus_imp_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus_imp_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}

reps=50

round(cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])),3)
round(cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])),3)
round(cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])),3)


round(cbind(colMeans(meanbias12[1:reps,]),colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,])),3)
round(cbind(colMeans(bias12[1:reps,]),colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,])),3)
round(cbind(colMeans(rmse12[1:reps,]),colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,])),3)

# SE
for (rep in 1:50){
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_concurrent_shorttest_hard_clus_imp_lowcor",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian3_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian2_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore1_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore2_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore3_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore4_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}
SE11=SE11[-c(6,12,33,48),]
SE21=SE21[-c(6,12,33,48),]
SE31=SE31[-c(6,12,33,48),]
SE12=SE12[-c(6,12,33,48),]
SE22=SE22[-c(6,12,33,48),]
SE32=SE32[-c(6,12,33,48),]
reps=46
round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)


##################
#   clustering
##################
reps=50
a1=a2=a3=0
for (rep in 1:reps){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus_imp_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus_imp_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,3]-Trait[,2]
  change.sco2=cbind(1:N,Trait[,3]-Trait[,2])
  grp1=change.sco2[which(change.sco<(-0.75)),]
  grp2=change.sco2[which((change.sco<=0.75)&(change.sco>-0.75)),]
  grp3=change.sco2[which(change.sco>0.75),]
  
  Trait10=readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_concurrent_shorttest_hard_clus_imp_lowcor",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian3_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_bayesian2_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore1_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore2_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore3_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation3_clus/sim2_shorttest_ignore4_hard_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  
  clus11=clus21=clus31=clus12=clus22=clus32=rep(2,800)
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  km11 <- kmeans( Trait11[,2]-Trait11[,1], 3, nstart=5000)
  cluster11=km11$cluster
  m1=mean((Trait11[,2]-Trait11[,1])[which( cluster11==1)])
  m2=mean((Trait11[,2]-Trait11[,1])[which( cluster11==2)])
  m3=mean((Trait11[,2]-Trait11[,1])[which( cluster11==3)])
  clus11[which( cluster11==which.min(c(m1,m2,m3)))]=1
  clus11[which( cluster11==which.max(c(m1,m2,m3)))]=3
  
  km21 <- kmeans((Trait21[,2]-Trait21[,1]), 3, nstart=5000)
  cluster21=km21$cluster
  m1=mean((Trait21[,2]-Trait21[,1])[which( cluster21==1)])
  m2=mean((Trait21[,2]-Trait21[,1])[which( cluster21==2)])
  m3=mean((Trait21[,2]-Trait21[,1])[which( cluster21==3)])
  clus21[which( cluster21==which.min(c(m1,m2,m3)))]=1
  clus21[which( cluster21==which.max(c(m1,m2,m3)))]=3
  
  km31 <- kmeans( Trait31[,2]-Trait31[,1], 3, nstart=5000)
  cluster31=km31$cluster
  m1=mean((Trait31[,2]-Trait31[,1])[which( cluster31==1)])
  m2=mean((Trait31[,2]-Trait31[,1])[which( cluster31==2)])
  m3=mean((Trait31[,2]-Trait31[,1])[which( cluster31==3)])
  clus31[which( cluster31==which.min(c(m1,m2,m3)))]=1
  clus31[which( cluster31==which.max(c(m1,m2,m3)))]=3
  
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  km12 <- kmeans( Trait12[,2]-Trait12[,1], 3, nstart=5000)
  cluster12=km12$cluster
  m1=mean((Trait12[,2]-Trait12[,1])[which( cluster12==1)])
  m2=mean((Trait12[,2]-Trait12[,1])[which( cluster12==2)])
  m3=mean((Trait12[,2]-Trait12[,1])[which( cluster12==3)])
  clus12[which( cluster12==which.min(c(m1,m2,m3)))]=1
  clus12[which( cluster12==which.max(c(m1,m2,m3)))]=3
  
  km22 <- kmeans((Trait22[,2]-Trait22[,1]), 3, nstart=5000)
  cluster22=km22$cluster
  m1=mean((Trait22[,2]-Trait22[,1])[which( cluster22==1)])
  m2=mean((Trait22[,2]-Trait22[,1])[which( cluster22==2)])
  m3=mean((Trait22[,2]-Trait22[,1])[which( cluster22==3)])
  clus22[which( cluster22==which.min(c(m1,m2,m3)))]=1
  clus22[which( cluster22==which.max(c(m1,m2,m3)))]=3
  
  
  km32 <- kmeans( Trait32[,2]-Trait32[,1], 3, nstart=5000)
  cluster32=km32$cluster
  m1=mean((Trait32[,2]-Trait32[,1])[which( cluster32==1)])
  m2=mean((Trait32[,2]-Trait32[,1])[which( cluster32==2)])
  m3=mean((Trait32[,2]-Trait32[,1])[which( cluster32==3)])
  clus32[which( cluster32==which.min(c(m1,m2,m3)))]=1
  clus32[which( cluster32==which.max(c(m1,m2,m3)))]=3
  
  
  
  grp11=(1:N)[which(c(clus11,clus12)==1)]
  grp12=(1:N)[which(c(clus11,clus12)==2)]
  grp13=(1:N)[which(c(clus11,clus12)==3)]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which(c(clus21,clus22)==1)]
  grp22=(1:N)[which(c(clus21,clus22)==2)]
  grp23=(1:N)[which(c(clus21,clus22)==3)]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which(c(clus31,clus32)==1)]
  grp32=(1:N)[which(c(clus31,clus32)==2)]
  grp33=(1:N)[which(c(clus31,clus32)==3)]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
reps=50
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


# plots 


library(ggplot2)

visit.cohot=c(rep("C1 V1",3),rep("C1 V2",3),rep("C1 CS",3),rep("C2 V1",3),rep("C2 V2",3),rep("C2 CS",3))
bias=c(0.007,	-0.018,	0.007,	0.027,	-0.040,	-0.020,0.020,	-0.022,	-0.027,0.005,	-0.024,	-0.003,	0.036,	-0.055,	-0.012,0.031,	-0.031,	-0.009)
Method=rep(c("Concurrent","Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$bias=as.numeric(data$bias)
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")+ geom_hline(yintercept = 0)
p1

rmse=c(0.383,	0.384,	0.435,	0.409,	0.409,	0.450,0.408,	0.409,	0.545,0.389,	0.391,	0.441,	0.421,	0.421,	0.463,0.411,	0.411,	0.543)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$rmse=as.numeric(data$rmse)
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.303,	0.303,	0.344,	0.324,	0.324,	0.354,0.328,	0.330,	0.434,0.307,	0.308,	0.348,	0.332,	0.332,	0.365,0.332,	0.332,	0.432)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$absbias=as.numeric(data$absbias)
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3


SEs=c(0.368,	0.363,	0.413,	0.398,	0.381,	0.426,0.543,	0.527,	0.596,0.372,	0.371,	0.420,	0.403,	0.394,	0.433,0.550,	0.542,	0.605)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$SEs=as.numeric(data$SEs)
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4


library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")


############################
# 
#         Sim2.3
#
############################
N1=N2=800
reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)


SE11=SE21=SE31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)
for (rep in 1:50){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_clus_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian3_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian2_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore1_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore2_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore3_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore4_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  # #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_clus_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_clus_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  #meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}


cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))

cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))

cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))

# SE
for (rep in 1:50){
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian3_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian2_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore1_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore2_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore3_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore4_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  #Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  #SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  #SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}

round(cbind(colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)


##################
#   clustering
##################

a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_clus_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_clus_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,3]-Trait[,2]
  change.sco2=cbind(1:N,Trait[,3]-Trait[,2])
  grp1=change.sco2[which(change.sco<(-0.75)),]
  grp2=change.sco2[which((change.sco<=0.75)&(change.sco>-0.75)),]
  grp3=change.sco2[which(change.sco>0.75),]
  
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian3_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian2_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore1_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore2_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore3_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore4_adnilan_clus_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  
  clus11=clus21=clus31=clus12=clus22=clus32=rep(2,800)
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #km11 <- kmeans( Trait11[,2]-Trait11[,1], 3, nstart=5000)
  #cluster11=km11$cluster
  #m1=mean((Trait11[,2]-Trait11[,1])[which( cluster11==1)])
  #m2=mean((Trait11[,2]-Trait11[,1])[which( cluster11==2)])
  #m3=mean((Trait11[,2]-Trait11[,1])[which( cluster11==3)])
  #clus11[which( cluster11==which.min(c(m1,m2,m3)))]=1
  #clus11[which( cluster11==which.max(c(m1,m2,m3)))]=3
  
  km21 <- kmeans((Trait21[,2]-Trait21[,1]), 3, nstart=5000)
  cluster21=km21$cluster
  m1=mean((Trait21[,2]-Trait21[,1])[which( cluster21==1)])
  m2=mean((Trait21[,2]-Trait21[,1])[which( cluster21==2)])
  m3=mean((Trait21[,2]-Trait21[,1])[which( cluster21==3)])
  clus21[which( cluster21==which.min(c(m1,m2,m3)))]=1
  clus21[which( cluster21==which.max(c(m1,m2,m3)))]=3
  
  km31 <- kmeans( Trait31[,2]-Trait31[,1], 3, nstart=5000)
  cluster31=km31$cluster
  m1=mean((Trait31[,2]-Trait31[,1])[which( cluster31==1)])
  m2=mean((Trait31[,2]-Trait31[,1])[which( cluster31==2)])
  m3=mean((Trait31[,2]-Trait31[,1])[which( cluster31==3)])
  clus31[which( cluster31==which.min(c(m1,m2,m3)))]=1
  clus31[which( cluster31==which.max(c(m1,m2,m3)))]=3
  
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #km12 <- kmeans( Trait12[,2]-Trait12[,1], 3, nstart=5000)
  #cluster12=km12$cluster
  #m1=mean((Trait12[,2]-Trait12[,1])[which( cluster12==1)])
  #m2=mean((Trait12[,2]-Trait12[,1])[which( cluster12==2)])
  #m3=mean((Trait12[,2]-Trait12[,1])[which( cluster12==3)])
  #clus12[which( cluster12==which.min(c(m1,m2,m3)))]=1
  #clus12[which( cluster12==which.max(c(m1,m2,m3)))]=3
  
  km22 <- kmeans((Trait22[,2]-Trait22[,1]), 3, nstart=5000)
  cluster22=km22$cluster
  m1=mean((Trait22[,2]-Trait22[,1])[which( cluster22==1)])
  m2=mean((Trait22[,2]-Trait22[,1])[which( cluster22==2)])
  m3=mean((Trait22[,2]-Trait22[,1])[which( cluster22==3)])
  clus22[which( cluster22==which.min(c(m1,m2,m3)))]=1
  clus22[which( cluster22==which.max(c(m1,m2,m3)))]=3
  
  
  km32 <- kmeans( Trait32[,2]-Trait32[,1], 3, nstart=5000)
  cluster32=km32$cluster
  m1=mean((Trait32[,2]-Trait32[,1])[which( cluster32==1)])
  m2=mean((Trait32[,2]-Trait32[,1])[which( cluster32==2)])
  m3=mean((Trait32[,2]-Trait32[,1])[which( cluster32==3)])
  clus32[which( cluster32==which.min(c(m1,m2,m3)))]=1
  clus32[which( cluster32==which.max(c(m1,m2,m3)))]=3
  
  
  
  #grp11=(1:N)[which(c(clus11,clus12)==1)]
  #grp12=(1:N)[which(c(clus11,clus12)==2)]
  #grp13=(1:N)[which(c(clus11,clus12)==3)]
  
  #a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which(c(clus21,clus22)==1)]
  grp22=(1:N)[which(c(clus21,clus22)==2)]
  grp23=(1:N)[which(c(clus21,clus22)==3)]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which(c(clus31,clus32)==1)]
  grp32=(1:N)[which(c(clus31,clus32)==2)]
  grp33=(1:N)[which(c(clus31,clus32)==3)]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
reps=50
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


# plots 


library(ggplot2)
visit.cohot=c(rep("C1 V1",2),rep("C1 V2",2),rep("C1 CS",2),rep("C2 V1",2),rep("C2 V2",2),rep("C2 CS",2))
bias=c(-0.013,	-0.006,	-0.021,	0.000,-0.008,	0.006,-0.026,	-0.008,	-0.021,	-0.013,0.005,	-0.005)
Method=rep(c("Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$bias=as.numeric(data$bias)
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")+ geom_hline(yintercept = 0)
p1

rmse=c(0.457,	0.495,	0.495	,0.522,0.408,	0.520,0.489,	0.523,	0.526,	0.546,0.410,	0.514)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$rmse=as.numeric(data$rmse)
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.363,	0.393,	0.394,	0.413,0.329,	0.414,0.390,	0.417,	0.420,	0.436,0.331,	0.409)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$absbias=as.numeric(data$absbias)
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3


SEs=c(0.450,	0.484,	0.491,	0.497,0.667,	0.696,0.455,	0.503,	0.496,	0.521,0.674,	0.725)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$SEs=as.numeric(data$SEs)
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4


library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")


# item parameters
meanbias1=meanbias2=meanbias3=matrix(0,reps,2)
bias1=bias2=bias3=matrix(0,reps,2)
rmse1=rmse2=rmse3=matrix(0,reps,2)

set.seed(1)
Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
J=9
set.seed(1)
grb1<-runif(J,-3,-1)
grb2<-runif(J,-1,1)
grb3<-runif(J,1,3)


grb<-matrix(c(grb1,grb2,grb3),nrow=J)
grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
grorb
round(grorb,3)
for (rep in 1:50){
  #a1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(1:6,10:12),3] # no constraint
  a2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  a3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  
  #d1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3] # no constraint
  d2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(61:78),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(70:78),3])
  d3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(8:25),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_lowcor_",rep,".out"))$parameters$unstandardized[c(17:25),3])
  
  a=c(Amat1[1:6],Amat2[4:6])
  b=matrix(t(grorb),27,1)
  
  # meanbias1[rep,1]=mean(abs(a1-a))
  meanbias2[rep,1]=mean(abs(a2-a))
  meanbias3[rep,1]=mean(abs(a3-a))
  
  # meanbias1[rep,2]=colMeans(abs(d1-b))
  meanbias2[rep,2]=colMeans(abs(d2-b))
  meanbias3[rep,2]=colMeans(abs(d3-b))
  
  #bias1[rep,1]=mean(a1-a)
  bias2[rep,1]=mean(a2-a)
  bias3[rep,1]=mean(a3-a)
  
  #bias1[rep,2]=mean(d1-b)
  bias2[rep,2]=mean(d2-b)
  bias3[rep,2]=mean(d3-b)
  
  #rmse1[rep,1]=sqrt(mean((a1-a)^2))
  rmse2[rep,1]=sqrt(mean((a2-a)^2))
  rmse3[rep,1]=sqrt(mean((a3-a)^2))
  
  #rmse1[rep,2]=sqrt(mean((d1-b)^2))
  rmse2[rep,2]=sqrt(mean((d2-b)^2))
  rmse3[rep,2]=sqrt(mean((d3-b)^2))
}
round(c(colMeans(meanbias2[1:reps,]),colMeans(meanbias3[1:reps,])),3)

round(c(colMeans(bias2[1:reps,]),colMeans(bias3[1:reps,])),3)

round(c(colMeans(rmse2[1:reps,]),colMeans(rmse3[1:reps,])),3)


#####################################################
# 
#         Sim2.4
#
#####################################################
N1=N2=800
reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)


SE11=SE21=SE31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)
for (rep in 1:50){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_clus_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian3_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian2_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore1_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore2_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore3_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore4_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  # #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_clus_imp_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_clus_imp_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  #meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
}

meanbias11=meanbias11[-36,]
meanbias21=meanbias21[-36,]
meanbias31=meanbias31[-36,]

bias11=bias11[-36,]
bias21=bias21[-36,]
bias31=bias31[-36,]

rmse11=rmse11[-36,]
rmse21=rmse21[-36,]
rmse31=rmse31[-36,]


meanbias12=meanbias12[-36,]
meanbias22=meanbias22[-36,]
meanbias32=meanbias32[-36,]

bias12=bias12[-36,]
bias22=bias22[-36,]
bias32=bias32[-36,]

rmse12=rmse12[-36,]
rmse22=rmse22[-36,]
rmse32=rmse32[-36,]

reps=50
cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))

cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))

cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))

# SE
for (rep in 1:50){
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian3_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian2_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore1_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore2_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore3_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore4_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  #Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  
  #SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  #SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
}
SE21=SE21[-36,]
SE31=SE31[-36,]
SE22=SE22[-36,]
SE32=SE32[-36,]
reps=50
round(cbind(colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3)
round(cbind(colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3)


##################
#   clustering
##################
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_clus_imp_lowcor.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_clus_imp_lowcor.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,3]-Trait[,2]
  change.sco2=cbind(1:N,Trait[,3]-Trait[,2])
  grp1=change.sco2[which(change.sco<(-0.1)),]
  grp2=change.sco2[which((change.sco<=0.1)&(change.sco>-0.1)),]
  grp3=change.sco2[which(change.sco>0.1),]
  
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian3_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian2_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore1_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore2_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore3_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore4_adnilan_clus_imp_lowcor_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  
  clus11=clus21=clus31=clus12=clus22=clus32=rep(2,800)
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,3)]
  
  #km11 <- kmeans( Trait11[,2]-Trait11[,1], 3, nstart=5000)
  #cluster11=km11$cluster
  #m1=mean((Trait11[,2]-Trait11[,1])[which( cluster11==1)])
  #m2=mean((Trait11[,2]-Trait11[,1])[which( cluster11==2)])
  #m3=mean((Trait11[,2]-Trait11[,1])[which( cluster11==3)])
  #clus11[which( cluster11==which.min(c(m1,m2,m3)))]=1
  #clus11[which( cluster11==which.max(c(m1,m2,m3)))]=3
  
  km21 <- kmeans((Trait21[,2]-Trait21[,1]), 3, nstart=5000)
  cluster21=km21$cluster
  m1=mean((Trait21[,2]-Trait21[,1])[which( cluster21==1)])
  m2=mean((Trait21[,2]-Trait21[,1])[which( cluster21==2)])
  m3=mean((Trait21[,2]-Trait21[,1])[which( cluster21==3)])
  clus21[which( cluster21==which.min(c(m1,m2,m3)))]=1
  clus21[which( cluster21==which.max(c(m1,m2,m3)))]=3
  
  km31 <- kmeans( Trait31[,2]-Trait31[,1], 3, nstart=5000)
  cluster31=km31$cluster
  m1=mean((Trait31[,2]-Trait31[,1])[which( cluster31==1)])
  m2=mean((Trait31[,2]-Trait31[,1])[which( cluster31==2)])
  m3=mean((Trait31[,2]-Trait31[,1])[which( cluster31==3)])
  clus31[which( cluster31==which.min(c(m1,m2,m3)))]=1
  clus31[which( cluster31==which.max(c(m1,m2,m3)))]=3
  
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,4)]
  
  #km12 <- kmeans( Trait12[,2]-Trait12[,1], 3, nstart=5000)
  #cluster12=km12$cluster
  #m1=mean((Trait12[,2]-Trait12[,1])[which( cluster12==1)])
  #m2=mean((Trait12[,2]-Trait12[,1])[which( cluster12==2)])
  #m3=mean((Trait12[,2]-Trait12[,1])[which( cluster12==3)])
  #clus12[which( cluster12==which.min(c(m1,m2,m3)))]=1
  #clus12[which( cluster12==which.max(c(m1,m2,m3)))]=3
  
  km22 <- kmeans((Trait22[,2]-Trait22[,1]), 3, nstart=5000)
  cluster22=km22$cluster
  m1=mean((Trait22[,2]-Trait22[,1])[which( cluster22==1)])
  m2=mean((Trait22[,2]-Trait22[,1])[which( cluster22==2)])
  m3=mean((Trait22[,2]-Trait22[,1])[which( cluster22==3)])
  clus22[which( cluster22==which.min(c(m1,m2,m3)))]=1
  clus22[which( cluster22==which.max(c(m1,m2,m3)))]=3
  
  
  km32 <- kmeans( Trait32[,2]-Trait32[,1], 3, nstart=5000)
  cluster32=km32$cluster
  m1=mean((Trait32[,2]-Trait32[,1])[which( cluster32==1)])
  m2=mean((Trait32[,2]-Trait32[,1])[which( cluster32==2)])
  m3=mean((Trait32[,2]-Trait32[,1])[which( cluster32==3)])
  clus32[which( cluster32==which.min(c(m1,m2,m3)))]=1
  clus32[which( cluster32==which.max(c(m1,m2,m3)))]=3
  
  
  
  #grp11=(1:N)[which(c(clus11,clus12)==1)]
  #grp12=(1:N)[which(c(clus11,clus12)==2)]
  #grp13=(1:N)[which(c(clus11,clus12)==3)]
  
  #a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])
  
  
  grp21=(1:N)[which(c(clus21,clus22)==1)]
  grp22=(1:N)[which(c(clus21,clus22)==2)]
  grp23=(1:N)[which(c(clus21,clus22)==3)]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])
  
  grp31=(1:N)[which(c(clus31,clus32)==1)]
  grp32=(1:N)[which(c(clus31,clus32)==2)]
  grp33=(1:N)[which(c(clus31,clus32)==3)]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
reps=50
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


# plots 

library(ggplot2)
visit.cohot=c(rep("C1 V1",2),rep("C1 V2",2),rep("C1 CS",2),rep("C2 V1",2),rep("C2 V2",2),rep("C2 CS",2))
bias=c(-0.009,	-0.010,	-0.010,	0.009,-0.000,	0.020,-0.032,	0.005,	-0.030,	-0.006,0.002,	-0.010)
Method=rep(c("Three-stage","Single"),6)
data=as.data.frame(cbind(visit.cohot,bias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$bias=as.numeric(data$bias)
p1 <-ggplot(data, aes(x=visit.cohot, y=bias,  shape=Method)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Bias")+ theme(legend.position = "none")+ geom_hline(yintercept = 0)
p1

rmse=c(0.455,	0.493,	0.493,	0.518,0.408,	0.522,0.488,	0.523,	0.526,	0.550,0.410,	0.526)
data=as.data.frame(cbind(visit.cohot,rmse,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$rmse=as.numeric(data$rmse)
p2 <-ggplot(data, aes(x=visit.cohot, y=rmse,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "RMSE") 
p2


absbias=c(0.362,	0.391,	0.393,	0.412,0.329,	0.415,0.389,	0.416,	0.419,	0.437,0.332,	0.418)
data=as.data.frame(cbind(visit.cohot,absbias,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$absbias=as.numeric(data$absbias)
p3 <-ggplot(data, aes(x=visit.cohot, y=absbias,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean Absolute Bias")+ theme(legend.position = "none")
p3


SEs=c(0.451,	0.485,	0.496,	0.496,0.671,	0.695,0.460,	0.518,	0.502,	0.534,0.684,	0.745)
data=as.data.frame(cbind(visit.cohot,SEs,Method))
data$visit.cohot=factor(data$visit.cohot,levels = unique(data$visit.cohot))
data$Method=factor(data$Method,levels = unique(data$Method))
data$SEs=as.numeric(data$SEs)
p4 <-ggplot(data, aes(x=visit.cohot, y=SEs,  shape=Method)) +
  geom_point(size=3) + 
  scale_shape_manual(values=c( 16, 17))+
  labs(title=" ",
       x="Cohort*Visit", y = "Mean SE")+ theme(legend.position = "none")
p4


library(ggpubr)
ggarrange(p1, p2, p3, p4,
          common.legend = TRUE, legend = "right")



# item parameters
meanbias1=meanbias2=meanbias3=matrix(0,reps,2)
bias1=bias2=bias3=matrix(0,reps,2)
rmse1=rmse2=rmse3=matrix(0,reps,2)

set.seed(1)
Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
J=9
set.seed(1)
grb1<-runif(J,-3,-1)
grb2<-runif(J,-1,1)
grb3<-runif(J,1,3)


grb<-matrix(c(grb1,grb2,grb3),nrow=J)
grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
grorb
round(grorb,3)
for (rep in 1:10){
  #a1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(1:6,10:12),3] # no constraint
  a2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  a3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(1:6),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(4:6),3])
  
  #d1=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$parameters$unstandardized[c(34:51,61:69),3] # no constraint
  d2=c(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(61:78),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(70:78),3])
  d3=c(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(8:25),3],
       readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_",rep,".out"))$parameters$unstandardized[c(17:25),3])
  
  a=c(Amat1[1:6],Amat2[4:6])
  b=matrix(t(grorb),27,1)
  
  # meanbias1[rep,1]=mean(abs(a1-a))
  meanbias2[rep,1]=mean(abs(a2-a))
  meanbias3[rep,1]=mean(abs(a3-a))
  
  # meanbias1[rep,2]=colMeans(abs(d1-b))
  meanbias2[rep,2]=colMeans(abs(d2-b))
  meanbias3[rep,2]=colMeans(abs(d3-b))
  
  #bias1[rep,1]=mean(a1-a)
  bias2[rep,1]=mean(a2-a)
  bias3[rep,1]=mean(a3-a)
  
  #bias1[rep,2]=mean(d1-b)
  bias2[rep,2]=mean(d2-b)
  bias3[rep,2]=mean(d3-b)
  
  #rmse1[rep,1]=sqrt(mean((a1-a)^2))
  rmse2[rep,1]=sqrt(mean((a2-a)^2))
  rmse3[rep,1]=sqrt(mean((a3-a)^2))
  
  #rmse1[rep,2]=sqrt(mean((d1-b)^2))
  rmse2[rep,2]=sqrt(mean((d2-b)^2))
  rmse3[rep,2]=sqrt(mean((d3-b)^2))
}
round(c(colMeans(meanbias2[1:reps,]),colMeans(meanbias3[1:reps,])),3)

round(c(colMeans(bias2[1:reps,]),colMeans(bias3[1:reps,])),3)

round(c(colMeans(rmse2[1:reps,]),colMeans(rmse3[1:reps,])),3)

