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

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)
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

bs1=(cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs2=(cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs3=(cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))

mb1=abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb2=abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb3=abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))


cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))
cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))
cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias12[1:reps,]),colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias12[1:reps,]),colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse12[1:reps,]),colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))

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

bs1=(cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs2=(cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs3=(cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))

mb1=abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb2=abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb3=abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))


round(cbind(colMeans(meanbias11[11:reps,]),colMeans(meanbias21[11:reps,]),colMeans(meanbias31[11:reps,])),3)
round(cbind(colMeans(bias11[11:reps,]),colMeans(bias21[11:reps,]),colMeans(bias31[11:reps,])),3)
round(cbind(colMeans(rmse11[11:reps,]),colMeans(rmse21[11:reps,]),colMeans(rmse31[11:reps,])),3)


round(cbind(colMeans(meanbias12[11:reps,]),colMeans(meanbias22[11:reps,]),colMeans(meanbias32[11:reps,])),3)
round(cbind(colMeans(bias12[11:reps,]),colMeans(bias22[11:reps,]),colMeans(bias32[11:reps,])),3)
round(cbind(colMeans(rmse12[11:reps,]),colMeans(rmse22[11:reps,]),colMeans(rmse32[11:reps,])),3)

mat=rbind(cbind(colMeans(bias11[11:reps,]),colMeans(bias21[11:reps,]),colMeans(bias31[11:reps,]))[1,],)

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
N1=N2=800
reps=50
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)
for (rep in 1:50){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
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

bs1=(cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs2=(cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs3=(cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))

mb1=abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb2=abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb3=abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))


cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))

cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))

cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))


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
reps=10
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)
for (rep in 1:10){
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1Sim2ConcurrentADNILan/sim2_bayesian1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1Sim2BayesianADNILan/sim2_bayesian2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Condition1Sim2IgnoreADNILan/sim2_ignore4_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")])
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

bs1=(cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs2=(cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
bs3=(cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))

mb1=abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb2=abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))
mb3=abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))


cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,]))

cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,]))

cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,]))


cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,]))
cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,]))
cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,]))

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

