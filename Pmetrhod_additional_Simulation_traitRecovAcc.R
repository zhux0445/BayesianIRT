#####################################################
# 
#         Sim 3.1
#
#####################################################

N1=N2=N3=800
reps=50

#cohort1
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

SE11=SE21=SE31=matrix(0,reps,3)

#cohort2

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)

#cohort3

meanbias13=meanbias23=meanbias33=matrix(0,reps,3)

bias13=bias23=bias33=matrix(0,reps,3)

rmse13=rmse23=rmse33=matrix(0,reps,3)

SE13=SE23=SE33=matrix(0,reps,3)

setwd("F:/PsychMethod/sim31")

for (rep in 1:50){
  Trait10=readModels(paste0("sim31_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim31_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim31_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim31_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("sim31_ignore1_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim31_ignore2_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim31_ignore3_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim31_ignore4_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim31_ignore5_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim31_ignore6_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  Trait13=Trait10[1601:2400,] # no constraint
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('Condition31_trait_cohort1.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('Condition31_trait_cohort2.csv')[((rep-1)*N2+1):(rep*N2),],
              read.csv('Condition31_trait_cohort3.csv')[((rep-1)*N3+1):(rep*N3),])
  #cohort1
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #cohort2
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  
  #cohort3
  meanbias13[rep,]=colMeans(abs(cbind(Trait13,Trait13[,2]-Trait13[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias23[rep,]=colMeans(abs(cbind(Trait23,Trait23[,2]-Trait23[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias33[rep,]=colMeans(abs(cbind(Trait33,Trait33[,2]-Trait33[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  bias13[rep,]=colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias23[rep,]=colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias33[rep,]=colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  rmse13[rep,]=sqrt(colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse23[rep,]=sqrt(colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse33[rep,]=sqrt(colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  
}

sim31_result=NULL

sim31_result=rbind(sim31_result,cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])))
sim31_result=rbind(sim31_result,cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])))
sim31_result=rbind(sim31_result,cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])))

sim31_result=rbind(sim31_result,cbind(colMeans(meanbias12[1:reps,]),colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,])))
sim31_result=rbind(sim31_result,cbind(colMeans(bias12[1:reps,]),colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,])))
sim31_result=rbind(sim31_result,cbind(colMeans(rmse12[1:reps,]),colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,])))


sim31_result=rbind(sim31_result,cbind(colMeans(meanbias13[1:reps,]),colMeans(meanbias23[1:reps,]),colMeans(meanbias33[1:reps,])))
sim31_result=rbind(sim31_result,cbind(colMeans(bias13[1:reps,]),colMeans(bias23[1:reps,]),colMeans(bias33[1:reps,])))
sim31_result=rbind(sim31_result,cbind(colMeans(rmse13[1:reps,]),colMeans(rmse23[1:reps,]),colMeans(rmse33[1:reps,])))


# SE
for (rep in 1:50){
  Trait10=readModels(paste0("sim31_concurrent_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")] # no constraint
  Trait20=cbind(readModels(paste0("Sim31_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim31_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim31_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("sim31_ignore1_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim31_ignore2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim31_ignore3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim31_ignore4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim31_ignore5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim31_ignore6_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  Trait13=Trait10[1601:2400,] # no constraint
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  Trait13[,3]=sqrt((Trait13[,1]^2) +(Trait13[,2]^2))
  Trait23[,3]=sqrt((Trait23[,1]^2) +(Trait23[,2]^2))
  Trait33=cbind(Trait33,as.matrix(sqrt((Trait33[,1]^2) +(Trait33[,2]^2))))
  
  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
  
  SE13[rep,]=colMeans(Trait13)
  SE23[rep,]=colMeans(Trait23)
  SE33[rep,]=colMeans(Trait33)
}

sim31_result=rbind(sim31_result,round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3))
sim31_result=rbind(sim31_result,round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3))
sim31_result=rbind(sim31_result,round(cbind(colMeans(SE13[1:reps,]),colMeans(SE23[1:reps,]),colMeans(SE33[1:reps,])),3))

write.csv(sim31_result,file = 'sim31_result.csv')


#####################################################
# 
#         Sim 3.2
#
#####################################################

N1=N2=N3=800
reps=50

#cohort1
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

SE11=SE21=SE31=matrix(0,reps,3)

#cohort2

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)

#cohort3

meanbias13=meanbias23=meanbias33=matrix(0,reps,3)

bias13=bias23=bias33=matrix(0,reps,3)

rmse13=rmse23=rmse33=matrix(0,reps,3)

SE13=SE23=SE33=matrix(0,reps,3)

setwd("F:/PsychMethod/sim32")

for (rep in 1:50){
  Trait10=readModels(paste0("sim32_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim32_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim32_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim32_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("sim32_ignore1_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim32_ignore2_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim32_ignore3_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim32_ignore4_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim32_ignore5_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim32_ignore6_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  Trait13=Trait10[1601:2400,] # no constraint
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('Condition32_trait_cohort1.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('Condition32_trait_cohort2.csv')[((rep-1)*N2+1):(rep*N2),],
              read.csv('Condition32_trait_cohort3.csv')[((rep-1)*N3+1):(rep*N3),])
  
  
  
  plot(Trait11[,1],Trait[1:800,2])
  abline(0,1)
  
  plot(Trait12[,1],Trait[801:1600,2])
  abline(0,1)
  
  
  plot(Trait13[,1],Trait[1601:2400,2])
  abline(0,1)
  
  
  plot(Trait11[,2],Trait[1:800,3])
  abline(0,1)
  
  plot(Trait12[,2],Trait[801:1600,3])
  abline(0,1)
  
  
  plot(Trait13[,2],Trait[1601:2400,3])
  abline(0,1)
  
  plot(Trait11[,2]-Trait11[,1],Trait[1:800,3]-Trait[1:800,2])
  abline(0,1)
  
  plot(Trait12[,2]-Trait12[,1],Trait[801:1600,3]-Trait[801:1600,2])
  abline(0,1)
  
  
  plot(Trait13[,2]-Trait13[,1],Trait[1601:2400,3]-Trait[1601:2400,2])
  abline(0,1)
  
  
  mean(abs((Trait11[,2]-Trait11[,1])-(Trait[1:800,3]-Trait[1:800,2])))
  mean(abs((Trait12[,2]-Trait12[,1])-(Trait[801:1600,3]-Trait[801:1600,2])))
  mean(abs((Trait13[,2]-Trait13[,1])-(Trait[1601:2400,3]-Trait[1601:2400,2])))
  
  
  
  #cohort1
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #cohort2
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  
  #cohort3
  meanbias13[rep,]=colMeans(abs(cbind(Trait13,Trait13[,2]-Trait13[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias23[rep,]=colMeans(abs(cbind(Trait23,Trait23[,2]-Trait23[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias33[rep,]=colMeans(abs(cbind(Trait33,Trait33[,2]-Trait33[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  bias13[rep,]=colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias23[rep,]=colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias33[rep,]=colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  rmse13[rep,]=sqrt(colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse23[rep,]=sqrt(colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse33[rep,]=sqrt(colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  
}

sim32_result=NULL
                   
sim32_result=rbind(sim32_result,cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])))
sim32_result=rbind(sim32_result,cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])))
sim32_result=rbind(sim32_result,cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])))


sim32_result=rbind(sim32_result,cbind(colMeans(meanbias12[1:reps,]),colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,])))
sim32_result=rbind(sim32_result,cbind(colMeans(bias12[1:reps,]),colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,])))
sim32_result=rbind(sim32_result,cbind(colMeans(rmse12[1:reps,]),colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,])))

sim32_result=rbind(sim32_result,cbind(colMeans(meanbias13[1:reps,]),colMeans(meanbias23[1:reps,]),colMeans(meanbias33[1:reps,])))
sim32_result=rbind(sim32_result,cbind(colMeans(bias13[1:reps,]),colMeans(bias23[1:reps,]),colMeans(bias33[1:reps,])))
sim32_result=rbind(sim32_result,cbind(colMeans(rmse13[1:reps,]),colMeans(rmse23[1:reps,]),colMeans(rmse33[1:reps,])))


# SE
for (rep in 1:50){
  Trait10=readModels(paste0("sim32_concurrent_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")] # no constraint
  Trait20=cbind(readModels(paste0("Sim32_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim32_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim32_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("sim32_ignore1_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim32_ignore2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim32_ignore3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim32_ignore4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim32_ignore5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim32_ignore6_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  Trait13=Trait10[1601:2400,] # no constraint
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  Trait13[,3]=sqrt((Trait13[,1]^2) +(Trait13[,2]^2))
  Trait23[,3]=sqrt((Trait23[,1]^2) +(Trait23[,2]^2))
  Trait33=cbind(Trait33,as.matrix(sqrt((Trait33[,1]^2) +(Trait33[,2]^2))))
  
  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
  
  SE13[rep,]=colMeans(Trait13)
  SE23[rep,]=colMeans(Trait23)
  SE33[rep,]=colMeans(Trait33)
}

sim32_result=rbind(sim32_result,round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3))
sim32_result=rbind(sim32_result,round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3))
sim32_result=rbind(sim32_result,round(cbind(colMeans(SE13[1:reps,]),colMeans(SE23[1:reps,]),colMeans(SE33[1:reps,])),3))

write.csv(sim32_result,file = 'sim32_result.csv')



#####################################################
# 
#         Sim 3.3
#
#####################################################

N1=N2=N3=800
reps=50

#cohort1
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

SE11=SE21=SE31=matrix(0,reps,3)

#cohort2

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)

#cohort3

meanbias13=meanbias23=meanbias33=matrix(0,reps,3)

bias13=bias23=bias33=matrix(0,reps,3)

rmse13=rmse23=rmse33=matrix(0,reps,3)

SE13=SE23=SE33=matrix(0,reps,3)

setwd("F:/PsychMethod/sim33")

for (rep in 1:50){
  #Trait10=readModels(paste0("sim33_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim33_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim33_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim33_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("sim33_ignore1_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim33_ignore2_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim33_ignore3_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim33_ignore4_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim33_ignore5_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim33_ignore6_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('Condition33_trait_cohort1.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('Condition33_trait_cohort2.csv')[((rep-1)*N2+1):(rep*N2),],
              read.csv('Condition33_trait_cohort3.csv')[((rep-1)*N3+1):(rep*N3),])
  #cohort1
  #meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #cohort2
  #meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  
  #cohort3
  #meanbias13[rep,]=colMeans(abs(cbind(Trait13,Trait13[,2]-Trait13[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias23[rep,]=colMeans(abs(cbind(Trait23,Trait23[,2]-Trait23[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias33[rep,]=colMeans(abs(cbind(Trait33,Trait33[,2]-Trait33[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  #bias13[rep,]=colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias23[rep,]=colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias33[rep,]=colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  #rmse13[rep,]=sqrt(colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse23[rep,]=sqrt(colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse33[rep,]=sqrt(colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  
}


sim33_result=NULL

sim33_result=rbind(sim33_result,cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])))
sim33_result=rbind(sim33_result,cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])))
sim33_result=rbind(sim33_result,cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])))


sim33_result=rbind(sim33_result,cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,])))
sim33_result=rbind(sim33_result,cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,])))
sim33_result=rbind(sim33_result,cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,])))

sim33_result=rbind(sim33_result,cbind(colMeans(meanbias23[1:reps,]),colMeans(meanbias33[1:reps,])))
sim33_result=rbind(sim33_result,cbind(colMeans(bias23[1:reps,]),colMeans(bias33[1:reps,])))
sim33_result=rbind(sim33_result,cbind(colMeans(rmse23[1:reps,]),colMeans(rmse33[1:reps,])))

# SE
for (rep in 1:50){
  #Trait10=readModels(paste0("sim33_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim33_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim33_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim33_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("sim33_ignore1_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim33_ignore2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim33_ignore3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim33_ignore4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim33_ignore5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim33_ignore6_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  #Trait13=Trait10[1601:2400,] # no constraint
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  #Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  #Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  #Trait13[,3]=sqrt((Trait13[,1]^2) +(Trait13[,2]^2))
  Trait23[,3]=sqrt((Trait23[,1]^2) +(Trait23[,2]^2))
  Trait33=cbind(Trait33,as.matrix(sqrt((Trait33[,1]^2) +(Trait33[,2]^2))))
  
  #SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  #SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
  
  #SE13[rep,]=colMeans(Trait13)
  SE23[rep,]=colMeans(Trait23)
  SE33[rep,]=colMeans(Trait33)
}


sim33_result=rbind(sim33_result,round(cbind(colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3))
sim33_result=rbind(sim33_result,round(cbind(colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3))
sim33_result=rbind(sim33_result,round(cbind(colMeans(SE23[1:reps,]),colMeans(SE33[1:reps,])),3))

write.csv(sim33_result,file = 'sim33_result.csv')



#####################################################
# 
#         Sim 3.4
#
#####################################################

N1=N2=N3=800
reps=50

#cohort1
meanbias11=meanbias21=meanbias31=matrix(0,reps,3)

bias11=bias21=bias31=matrix(0,reps,3)

rmse11=rmse21=rmse31=matrix(0,reps,3)

SE11=SE21=SE31=matrix(0,reps,3)

#cohort2

meanbias12=meanbias22=meanbias32=matrix(0,reps,3)

bias12=bias22=bias32=matrix(0,reps,3)

rmse12=rmse22=rmse32=matrix(0,reps,3)

SE12=SE22=SE32=matrix(0,reps,3)

#cohort3

meanbias13=meanbias23=meanbias33=matrix(0,reps,3)

bias13=bias23=bias33=matrix(0,reps,3)

rmse13=rmse23=rmse33=matrix(0,reps,3)

SE13=SE23=SE33=matrix(0,reps,3)

setwd("F:/PsychMethod/sim34")

for (rep in 1:50){
  #Trait10=readModels(paste0("sim33_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim34_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim34_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("Sim34_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("sim34_ignore1_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim34_ignore2_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim34_ignore3_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim34_ignore4_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim34_ignore5_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim34_ignore6_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('Condition34_trait_cohort1.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('Condition34_trait_cohort2.csv')[((rep-1)*N2+1):(rep*N2),],
              read.csv('Condition34_trait_cohort3.csv')[((rep-1)*N3+1):(rep*N3),])
  #cohort1
  #meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1])))
  
  #rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1]))^2))
  
  #cohort2
  #meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1])))
  
  #rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1]))^2))
  
  #cohort3
  #meanbias13[rep,]=colMeans(abs(cbind(Trait13,Trait13[,2]-Trait13[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias23[rep,]=colMeans(abs(cbind(Trait23,Trait23[,2]-Trait23[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  meanbias33[rep,]=colMeans(abs(cbind(Trait33,Trait33[,2]-Trait33[,1])- cbind( Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  #bias13[rep,]=colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias23[rep,]=colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  bias33[rep,]=colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1])))
  
  #rmse13[rep,]=sqrt(colMeans((cbind(Trait13,Trait13[,2]-Trait13[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse23[rep,]=sqrt(colMeans((cbind(Trait23,Trait23[,2]-Trait23[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  rmse33[rep,]=sqrt(colMeans((cbind(Trait33,Trait33[,2]-Trait33[,1])-  cbind(  Trait[1601:2400,-1],(Trait[1601:2400,-1])[,2]-(Trait[1601:2400,-1])[,1]))^2))
  
}


sim34_result=NULL

sim34_result=rbind(sim34_result,cbind(colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])))
sim34_result=rbind(sim34_result,cbind(colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])))
sim34_result=rbind(sim34_result,cbind(colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])))


sim34_result=rbind(sim34_result,cbind(colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,])))
sim34_result=rbind(sim34_result,cbind(colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,])))
sim34_result=rbind(sim34_result,cbind(colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,])))

sim34_result=rbind(sim34_result,cbind(colMeans(meanbias23[1:reps,]),colMeans(meanbias33[1:reps,])))
sim34_result=rbind(sim34_result,cbind(colMeans(bias23[1:reps,]),colMeans(bias33[1:reps,])))
sim34_result=rbind(sim34_result,cbind(colMeans(rmse23[1:reps,]),colMeans(rmse33[1:reps,])))


# SE
for (rep in 1:50){
  #Trait10=readModels(paste0("sim33_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim34_Bayesian5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim34_Bayesian4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")],
                readModels(paste0("Sim34_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("sim34_ignore1_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim34_ignore2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim34_ignore3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim34_ignore4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim34_ignore5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim34_ignore6_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  #cohort1
  #Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:2]
  Trait31=Trait30[,c(1,4)]
  
  #cohort2
  #Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,3:4]
  Trait32=Trait30[,c(2,5)]
  
  #cohort3
  #Trait13=Trait10[1601:2400,] # no constraint
  Trait23=Trait20[,5:6]
  Trait33=Trait30[,c(3,6)]
  
  #Trait11[,3]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,3]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  #Trait12[,3]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,3]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  #Trait13[,3]=sqrt((Trait13[,1]^2) +(Trait13[,2]^2))
  Trait23[,3]=sqrt((Trait23[,1]^2) +(Trait23[,2]^2))
  Trait33=cbind(Trait33,as.matrix(sqrt((Trait33[,1]^2) +(Trait33[,2]^2))))
  
  #SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  #SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
  
  #SE13[rep,]=colMeans(Trait13)
  SE23[rep,]=colMeans(Trait23)
  SE33[rep,]=colMeans(Trait33)
}


sim34_result=rbind(sim34_result,round(cbind(colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3))
sim34_result=rbind(sim34_result,round(cbind(colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3))
sim34_result=rbind(sim34_result,round(cbind(colMeans(SE23[1:reps,]),colMeans(SE33[1:reps,])),3))

write.csv(sim34_result,file = 'sim34_result.csv')


#####################################################
# 
#         Sim 5.1
#
#####################################################

N1=N2=800
reps=50

#cohort1
meanbias11=meanbias21=meanbias31=matrix(0,reps,6)

bias11=bias21=bias31=matrix(0,reps,6)

rmse11=rmse21=rmse31=matrix(0,reps,6)

SE11=SE21=SE31=matrix(0,reps,6)

#cohort2

meanbias12=meanbias22=meanbias32=matrix(0,reps,6)

bias12=bias22=bias32=matrix(0,reps,6)

rmse12=rmse22=rmse32=matrix(0,reps,6)

SE12=SE22=SE32=matrix(0,reps,6)



setwd("F:/PsychMethod/sim51")

for (rep in 1:50){
  Trait10=readModels(paste0("sim51_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean","TH1_3.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim51_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean","TH1_3.Mean")],
                readModels(paste0("Sim51_Bayesian2_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean","TH1_3.Mean")])
  Trait30=cbind(readModels(paste0("sim51_ignore1_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim51_ignore2_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim51_ignore3_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim51_ignore4_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim51_ignore5_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim51_ignore6_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:3]
  Trait31=Trait30[,c(1,3,5)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,4:6]
  Trait32=Trait30[,c(2,4,6)]
  
 
  
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('Condition51_trait_cohort1.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('Condition51_trait_cohort2.csv')[((rep-1)*N2+1):(rep*N2),])
  #cohort1
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1],Trait11[,3]-Trait11[,2],Trait11[,3]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1],Trait21[,3]-Trait21[,2],Trait21[,3]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1],Trait31[,3]-Trait31[,2],Trait31[,3]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1],Trait11[,3]-Trait11[,2],Trait11[,3]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1],Trait21[,3]-Trait21[,2],Trait21[,3]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1],Trait31[,3]-Trait31[,2],Trait31[,3]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1],Trait11[,3]-Trait11[,2],Trait11[,3]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1],Trait21[,3]-Trait21[,2],Trait21[,3]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1],Trait31[,3]-Trait31[,2],Trait31[,3]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1]))^2))
  
  #cohort2
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1],Trait12[,3]-Trait12[,2],Trait12[,3]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1],Trait22[,3]-Trait22[,2],Trait22[,3]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1],Trait32[,3]-Trait32[,2],Trait32[,3]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1],Trait12[,3]-Trait12[,2],Trait12[,3]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1],Trait22[,3]-Trait22[,2],Trait22[,3]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1],Trait32[,3]-Trait32[,2],Trait32[,3]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1],Trait12[,3]-Trait12[,2],Trait12[,3]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1],Trait22[,3]-Trait22[,2],Trait22[,3]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1],Trait32[,3]-Trait32[,2],Trait32[,3]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1]))^2))
  
}



sim51_result=NULL

sim51_result=rbind(sim51_result,cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])))
sim51_result=rbind(sim51_result,cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])))
sim51_result=rbind(sim51_result,cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])))

sim51_result=rbind(sim51_result,cbind(colMeans(meanbias12[1:reps,]),colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,])))
sim51_result=rbind(sim51_result,cbind(colMeans(bias12[1:reps,]),colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,])))
sim51_result=rbind(sim51_result,cbind(colMeans(rmse12[1:reps,]),colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,])))



# SE
for (rep in 1:50){
    Trait10=readModels(paste0("sim51_concurrent_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation","TH1_3.Standard.Deviation")] # no constraint
    Trait20=cbind(readModels(paste0("Sim51_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation","TH1_3.Standard.Deviation")],
                  readModels(paste0("Sim51_Bayesian2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation","TH1_3.Standard.Deviation")])
    Trait30=cbind(readModels(paste0("sim51_ignore1_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                  readModels(paste0("sim51_ignore2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                  readModels(paste0("sim51_ignore3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                  readModels(paste0("sim51_ignore4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                  readModels(paste0("sim51_ignore5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                  readModels(paste0("sim51_ignore6_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
    #cohort1
    Trait11=Trait10[1:800,] # no constraint
    Trait21=Trait20[,1:3]
    Trait31=Trait30[,c(1,3,5)]
    
    #cohort2
    Trait12=Trait10[801:1600,] # no constraint
    Trait22=Trait20[,4:6]
    Trait32=Trait30[,c(2,4,6)]
  

  Trait11[,4]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,4]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,4]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,4]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  Trait11[,5]=sqrt((Trait11[,2]^2) +(Trait11[,3]^2))
  Trait21[,5]=sqrt((Trait21[,2]^2) +(Trait21[,3]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,2]^2) +(Trait31[,3]^2))))
  
  Trait12[,5]=sqrt((Trait12[,2]^2) +(Trait12[,3]^2))
  Trait22[,5]=sqrt((Trait22[,2]^2) +(Trait22[,3]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,2]^2) +(Trait32[,3]^2))))
  
  Trait11[,6]=sqrt((Trait11[,1]^2) +(Trait11[,3]^2))
  Trait21[,6]=sqrt((Trait21[,1]^2) +(Trait21[,3]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,3]^2))))
  
  Trait12[,6]=sqrt((Trait12[,1]^2) +(Trait12[,3]^2))
  Trait22[,6]=sqrt((Trait22[,1]^2) +(Trait22[,3]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,3]^2))))

  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)

}

sim51_result=rbind(sim51_result,round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3))
sim51_result=rbind(sim51_result,round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3))

write.csv(sim51_result,file = 'sim51_result.csv')


#####################################################
# 
#         Sim 5.2
#
#####################################################
N1=N2=800
reps=50

#cohort1
meanbias11=meanbias21=meanbias31=matrix(0,reps,6)

bias11=bias21=bias31=matrix(0,reps,6)

rmse11=rmse21=rmse31=matrix(0,reps,6)

SE11=SE21=SE31=matrix(0,reps,6)

#cohort2

meanbias12=meanbias22=meanbias32=matrix(0,reps,6)

bias12=bias22=bias32=matrix(0,reps,6)

rmse12=rmse22=rmse32=matrix(0,reps,6)

SE12=SE22=SE32=matrix(0,reps,6)



setwd("F:/PsychMethod/sim52")

for (rep in 1:50){
  Trait10=readModels(paste0("sim52_concurrent_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean","TH1_3.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("Sim52_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean","TH1_3.Mean")],
                readModels(paste0("Sim52_Bayesian2_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean","TH1_3.Mean")])
  Trait30=cbind(readModels(paste0("sim52_ignore1_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim52_ignore2_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim52_ignore3_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim52_ignore4_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim52_ignore5_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("sim52_ignore6_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:3]
  Trait31=Trait30[,c(1,3,5)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,4:6]
  Trait32=Trait30[,c(2,4,6)]
  
  
  
  #Trait32=c(readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore1_",rep,".out"))$savedata[,c("TH1_2.Mean")],
  #          readModels(paste0("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim1Ignore/sim1Ignore2_",rep,".out"))$savedata[,c("TH1_2.Mean")])
  Trait=rbind(read.csv('Condition52_trait_cohort1.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('Condition52_trait_cohort2.csv')[((rep-1)*N2+1):(rep*N2),])
  #cohort1
  meanbias11[rep,]=colMeans(abs(cbind(Trait11,Trait11[,2]-Trait11[,1],Trait11[,3]-Trait11[,2],Trait11[,3]-Trait11[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  meanbias21[rep,]=colMeans(abs(cbind(Trait21,Trait21[,2]-Trait21[,1],Trait21[,3]-Trait21[,2],Trait21[,3]-Trait21[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  meanbias31[rep,]=colMeans(abs(cbind(Trait31,Trait31[,2]-Trait31[,1],Trait31[,3]-Trait31[,2],Trait31[,3]-Trait31[,1])- cbind( Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  
  bias11[rep,]=colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1],Trait11[,3]-Trait11[,2],Trait11[,3]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  bias21[rep,]=colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1],Trait21[,3]-Trait21[,2],Trait21[,3]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  bias31[rep,]=colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1],Trait31[,3]-Trait31[,2],Trait31[,3]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1])))
  
  rmse11[rep,]=sqrt(colMeans((cbind(Trait11,Trait11[,2]-Trait11[,1],Trait11[,3]-Trait11[,2],Trait11[,3]-Trait11[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1]))^2))
  rmse21[rep,]=sqrt(colMeans((cbind(Trait21,Trait21[,2]-Trait21[,1],Trait21[,3]-Trait21[,2],Trait21[,3]-Trait21[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1]))^2))
  rmse31[rep,]=sqrt(colMeans((cbind(Trait31,Trait31[,2]-Trait31[,1],Trait31[,3]-Trait31[,2],Trait31[,3]-Trait31[,1])-  cbind(  Trait[1:800,-1],(Trait[1:800,-1])[,2]-(Trait[1:800,-1])[,1],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,2],(Trait[1:800,-1])[,3]-(Trait[1:800,-1])[,1]))^2))
  
  #cohort2
  meanbias12[rep,]=colMeans(abs(cbind(Trait12,Trait12[,2]-Trait12[,1],Trait12[,3]-Trait12[,2],Trait12[,3]-Trait12[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  meanbias22[rep,]=colMeans(abs(cbind(Trait22,Trait22[,2]-Trait22[,1],Trait22[,3]-Trait22[,2],Trait22[,3]-Trait22[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  meanbias32[rep,]=colMeans(abs(cbind(Trait32,Trait32[,2]-Trait32[,1],Trait32[,3]-Trait32[,2],Trait32[,3]-Trait32[,1])- cbind( Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  
  bias12[rep,]=colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1],Trait12[,3]-Trait12[,2],Trait12[,3]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  bias22[rep,]=colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1],Trait22[,3]-Trait22[,2],Trait22[,3]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  bias32[rep,]=colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1],Trait32[,3]-Trait32[,2],Trait32[,3]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1])))
  
  rmse12[rep,]=sqrt(colMeans((cbind(Trait12,Trait12[,2]-Trait12[,1],Trait12[,3]-Trait12[,2],Trait12[,3]-Trait12[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1]))^2))
  rmse22[rep,]=sqrt(colMeans((cbind(Trait22,Trait22[,2]-Trait22[,1],Trait22[,3]-Trait22[,2],Trait22[,3]-Trait22[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1]))^2))
  rmse32[rep,]=sqrt(colMeans((cbind(Trait32,Trait32[,2]-Trait32[,1],Trait32[,3]-Trait32[,2],Trait32[,3]-Trait32[,1])-  cbind(Trait[801:1600,-1],(Trait[801:1600,-1])[,2]-(Trait[801:1600,-1])[,1],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,2],(Trait[801:1600,-1])[,3]-(Trait[801:1600,-1])[,1]))^2))
  
}



sim52_result=NULL

sim52_result=rbind(sim52_result,cbind(colMeans(meanbias11[1:reps,]),colMeans(meanbias21[1:reps,]),colMeans(meanbias31[1:reps,])))
sim52_result=rbind(sim52_result,cbind(colMeans(bias11[1:reps,]),colMeans(bias21[1:reps,]),colMeans(bias31[1:reps,])))
sim52_result=rbind(sim52_result,cbind(colMeans(rmse11[1:reps,]),colMeans(rmse21[1:reps,]),colMeans(rmse31[1:reps,])))

sim52_result=rbind(sim52_result,cbind(colMeans(meanbias12[1:reps,]),colMeans(meanbias22[1:reps,]),colMeans(meanbias32[1:reps,])))
sim52_result=rbind(sim52_result,cbind(colMeans(bias12[1:reps,]),colMeans(bias22[1:reps,]),colMeans(bias32[1:reps,])))
sim52_result=rbind(sim52_result,cbind(colMeans(rmse12[1:reps,]),colMeans(rmse22[1:reps,]),colMeans(rmse32[1:reps,])))



# SE
for (rep in 1:50){
  Trait10=readModels(paste0("sim52_concurrent_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation","TH1_3.Standard.Deviation")] # no constraint
  Trait20=cbind(readModels(paste0("Sim52_Bayesian3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation","TH1_3.Standard.Deviation")],
                readModels(paste0("Sim52_Bayesian2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation","TH1_2.Standard.Deviation","TH1_3.Standard.Deviation")])
  Trait30=cbind(readModels(paste0("sim52_ignore1_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim52_ignore2_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim52_ignore3_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim52_ignore4_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim52_ignore5_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")],
                readModels(paste0("sim52_ignore6_",rep,".out"))$savedata[,c("TH1_1.Standard.Deviation")])
  #cohort1
  Trait11=Trait10[1:800,] # no constraint
  Trait21=Trait20[,1:3]
  Trait31=Trait30[,c(1,3,5)]
  
  #cohort2
  Trait12=Trait10[801:1600,] # no constraint
  Trait22=Trait20[,4:6]
  Trait32=Trait30[,c(2,4,6)]
  
  
  Trait11[,4]=sqrt((Trait11[,1]^2) +(Trait11[,2]^2))
  Trait21[,4]=sqrt((Trait21[,1]^2) +(Trait21[,2]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,2]^2))))
  
  Trait12[,4]=sqrt((Trait12[,1]^2) +(Trait12[,2]^2))
  Trait22[,4]=sqrt((Trait22[,1]^2) +(Trait22[,2]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,2]^2))))
  
  Trait11[,5]=sqrt((Trait11[,2]^2) +(Trait11[,3]^2))
  Trait21[,5]=sqrt((Trait21[,2]^2) +(Trait21[,3]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,2]^2) +(Trait31[,3]^2))))
  
  Trait12[,5]=sqrt((Trait12[,2]^2) +(Trait12[,3]^2))
  Trait22[,5]=sqrt((Trait22[,2]^2) +(Trait22[,3]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,2]^2) +(Trait32[,3]^2))))
  
  Trait11[,6]=sqrt((Trait11[,1]^2) +(Trait11[,3]^2))
  Trait21[,6]=sqrt((Trait21[,1]^2) +(Trait21[,3]^2))
  Trait31=cbind(Trait31,as.matrix(sqrt((Trait31[,1]^2) +(Trait31[,3]^2))))
  
  Trait12[,6]=sqrt((Trait12[,1]^2) +(Trait12[,3]^2))
  Trait22[,6]=sqrt((Trait22[,1]^2) +(Trait22[,3]^2))
  Trait32=cbind(Trait32,as.matrix(sqrt((Trait32[,1]^2) +(Trait32[,3]^2))))
  
  SE11[rep,]=colMeans(Trait11)
  SE21[rep,]=colMeans(Trait21)
  SE31[rep,]=colMeans(Trait31)
  
  SE12[rep,]=colMeans(Trait12)
  SE22[rep,]=colMeans(Trait22)
  SE32[rep,]=colMeans(Trait32)
  
}

sim52_result=rbind(sim52_result,round(cbind(colMeans(SE11[1:reps,]),colMeans(SE21[1:reps,]),colMeans(SE31[1:reps,])),3))
sim52_result=rbind(sim52_result,round(cbind(colMeans(SE12[1:reps,]),colMeans(SE22[1:reps,]),colMeans(SE32[1:reps,])),3))

write.csv(sim52_result,file = 'sim52_result.csv')
