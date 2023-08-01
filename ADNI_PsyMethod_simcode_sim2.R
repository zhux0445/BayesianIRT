################
#    sim2.1 
################
reps=20
a1=a2=a3=0
for (rep in 1:20){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  
  Trait10=readModels(paste0("F:/PsychMethod/simulation3_clus/sim2_concurrent_shorttest_hard_clus",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/simulation3_clus/sim2_shorttest_bayesian3_hard_clus_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/simulation3_clus/sim2_shorttest_bayesian2_hard_clus_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/simulation3_clus/sim2_shorttest_ignore1_hard_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/simulation3_clus/sim2_shorttest_ignore2_hard_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/simulation3_clus/sim2_shorttest_ignore3_hard_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/simulation3_clus/sim2_shorttest_ignore4_hard_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")])
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_clus.csv')[((rep-1)*N1+1):(rep*N1),-1],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_clus.csv')[((rep-1)*N2+1):(rep*N2),-1])
  change=Trait[,3]-Trait[,2]
  Theta2n[which(Theta2n[,2]<sort(Theta2)[200]),3]
  km1 <- kmeans( Trait10[,2]-Trait10[,1], 3, nstart=5000)
  cluster1=km1$cluster
  km2 <- kmeans( c((Trait20[,2]-Trait20[,1]),(Trait20[,4]-Trait20[,3])), 3, nstart=5000)
  cluster2=km2$cluster
  km3 <- kmeans( c((Trait30[,2]-Trait30[,1]),(Trait30[,4]-Trait30[,3])), 3, nstart=5000)
  cluster3=km3$cluster
}

table(cluster1)
table(cluster2)
table(cluster3)
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[320]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[320*2])&(change.sco>sort(change.sco)[320])),]
  grp3=change.sco2[which((change.sco<=sort(change.sco)[320*3])&(change.sco>sort(change.sco)[320*2])),]
  grp4=change.sco2[which((change.sco<=sort(change.sco)[320*4])&(change.sco>sort(change.sco)[320*3])),]
  grp5=change.sco2[which((change.sco<=sort(change.sco)[320*5])&(change.sco>sort(change.sco)[320*4])),]
  
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
  
  grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[320])]
  grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*2])&((Trait10[,3])>sort(change.sco)[320]))]
  grp13=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*3])&((Trait10[,3])>sort(change.sco)[320*2]))]
  grp14=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*4])&((Trait10[,3])>sort(change.sco)[320*3]))]
  grp15=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*5])&((Trait10[,3])>sort(change.sco)[320*4]))]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])+sum(grp14%in%grp4[,1])+sum(grp15%in%grp5[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[320])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*2])&((Trait20[,3])>sort(change.sco)[320]))]
  grp23=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*3])&((Trait20[,3])>sort(change.sco)[320*2]))]
  grp24=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*4])&((Trait20[,3])>sort(change.sco)[320*3]))]
  grp25=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*5])&((Trait20[,3])>sort(change.sco)[320*4]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])+sum(grp24%in%grp4[,1])+sum(grp25%in%grp5[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[320])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*2])&((Trait30[,3])>sort(change.sco)[320]))]
  grp33=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*3])&((Trait30[,3])>sort(change.sco)[320*2]))]
  grp34=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*4])&((Trait30[,3])>sort(change.sco)[320*3]))]
  grp35=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*5])&((Trait30[,3])>sort(change.sco)[320*4]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])+sum(grp34%in%grp4[,1])+sum(grp35%in%grp5[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875

################
#    sim2.2 
################
reps=50
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_shorttest_hard_imp.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_shorttest_hard_imp.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[320]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[320*2])&(change.sco>sort(change.sco)[320])),]
  grp3=change.sco2[which((change.sco<=sort(change.sco)[320*3])&(change.sco>sort(change.sco)[320*2])),]
  grp4=change.sco2[which((change.sco<=sort(change.sco)[320*4])&(change.sco>sort(change.sco)[320*3])),]
  grp5=change.sco2[which((change.sco<=sort(change.sco)[320*5])&(change.sco>sort(change.sco)[320*4])),]
  
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
  
  grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[320])]
  grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*2])&((Trait10[,3])>sort(change.sco)[320]))]
  grp13=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*3])&((Trait10[,3])>sort(change.sco)[320*2]))]
  grp14=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*4])&((Trait10[,3])>sort(change.sco)[320*3]))]
  grp15=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*5])&((Trait10[,3])>sort(change.sco)[320*4]))]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])+sum(grp14%in%grp4[,1])+sum(grp15%in%grp5[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[320])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*2])&((Trait20[,3])>sort(change.sco)[320]))]
  grp23=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*3])&((Trait20[,3])>sort(change.sco)[320*2]))]
  grp24=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*4])&((Trait20[,3])>sort(change.sco)[320*3]))]
  grp25=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*5])&((Trait20[,3])>sort(change.sco)[320*4]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])+sum(grp24%in%grp4[,1])+sum(grp25%in%grp5[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[320])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*2])&((Trait30[,3])>sort(change.sco)[320]))]
  grp33=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*3])&((Trait30[,3])>sort(change.sco)[320*2]))]
  grp34=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*4])&((Trait30[,3])>sort(change.sco)[320*3]))]
  grp35=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*5])&((Trait30[,3])>sort(change.sco)[320*4]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])+sum(grp34%in%grp4[,1])+sum(grp35%in%grp5[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


################
#    sim2.3 
################
reps=50
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_clus.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_clus.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[320]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[320*2])&(change.sco>sort(change.sco)[320])),]
  grp3=change.sco2[which((change.sco<=sort(change.sco)[320*3])&(change.sco>sort(change.sco)[320*2])),]
  grp4=change.sco2[which((change.sco<=sort(change.sco)[320*4])&(change.sco>sort(change.sco)[320*3])),]
  grp5=change.sco2[which((change.sco<=sort(change.sco)[320*5])&(change.sco>sort(change.sco)[320*4])),]
  
  #Trait10=readModels(paste0("F:/PsychMethod/Condition1ShortHardSim2Concurrent/sim2_concurrent_shorttest_hard",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian3_adnilan_clus_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_bayesian2_adnilan_clus_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore1_adnilan_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore2_adnilan_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore3_adnilan_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_clus/sim2_ignore4_adnilan_clus_",rep,".out"))$savedata[,c("TH1_1.Mean")])
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
  
  grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[320])]
  grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*2])&((Trait10[,3])>sort(change.sco)[320]))]
  grp13=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*3])&((Trait10[,3])>sort(change.sco)[320*2]))]
  grp14=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*4])&((Trait10[,3])>sort(change.sco)[320*3]))]
  grp15=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*5])&((Trait10[,3])>sort(change.sco)[320*4]))]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])+sum(grp14%in%grp4[,1])+sum(grp15%in%grp5[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[320])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*2])&((Trait20[,3])>sort(change.sco)[320]))]
  grp23=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*3])&((Trait20[,3])>sort(change.sco)[320*2]))]
  grp24=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*4])&((Trait20[,3])>sort(change.sco)[320*3]))]
  grp25=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*5])&((Trait20[,3])>sort(change.sco)[320*4]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])+sum(grp24%in%grp4[,1])+sum(grp25%in%grp5[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[320])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*2])&((Trait30[,3])>sort(change.sco)[320]))]
  grp33=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*3])&((Trait30[,3])>sort(change.sco)[320*2]))]
  grp34=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*4])&((Trait30[,3])>sort(change.sco)[320*3]))]
  grp35=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*5])&((Trait30[,3])>sort(change.sco)[320*4]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])+sum(grp34%in%grp4[,1])+sum(grp35%in%grp5[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875

################
#    sim2.4 
################
reps=50
a1=a2=a3=0
for (rep in 1:50){
  Trait=rbind(read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort1_ADNILan_imp.csv')[((rep-1)*N1+1):(rep*N1),],
              read.csv('/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition2_trait_cohort2_ADNILan_imp.csv')[((rep-1)*N2+1):(rep*N2),])[,-1]
  change.sco=Trait[,2]-Trait[,1]
  change.sco2=cbind(1:N,Trait[,2]-Trait[,1])
  grp1=change.sco2[which(change.sco<=sort(change.sco)[320]),]
  grp2=change.sco2[which((change.sco<=sort(change.sco)[320*2])&(change.sco>sort(change.sco)[320])),]
  grp3=change.sco2[which((change.sco<=sort(change.sco)[320*3])&(change.sco>sort(change.sco)[320*2])),]
  grp4=change.sco2[which((change.sco<=sort(change.sco)[320*4])&(change.sco>sort(change.sco)[320*3])),]
  grp5=change.sco2[which((change.sco<=sort(change.sco)[320*5])&(change.sco>sort(change.sco)[320*4])),]
  
  #Trait10=readModels(paste0("F:/PsychMethod/Simulation3_imp/sim2_concurrent_shorttest_hard_imp",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")] # no constraint
  Trait20=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_bayesian2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean","TH1_2.Mean")])
  Trait30=cbind(readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore1_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore2_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore3_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")],
                readModels(paste0("F:/PsychMethod/Simulation6_imp/sim2_ignore4_adnilan_imp_",rep,".out"))$savedata[,c("TH1_1.Mean")])
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
  
  grp11=(1:N)[which((Trait10[,3])<=sort(change.sco)[320])]
  grp12=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*2])&((Trait10[,3])>sort(change.sco)[320]))]
  grp13=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*3])&((Trait10[,3])>sort(change.sco)[320*2]))]
  grp14=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*4])&((Trait10[,3])>sort(change.sco)[320*3]))]
  grp15=(1:N)[which(((Trait10[,3])<=sort(change.sco)[320*5])&((Trait10[,3])>sort(change.sco)[320*4]))]
  
  a1=a1+sum(grp11%in%grp1[,1])+sum(grp12%in%grp2[,1])+sum(grp13%in%grp3[,1])+sum(grp14%in%grp4[,1])+sum(grp15%in%grp5[,1])
  
  
  grp21=(1:N)[which((Trait20[,3])<=sort(change.sco)[320])]
  grp22=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*2])&((Trait20[,3])>sort(change.sco)[320]))]
  grp23=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*3])&((Trait20[,3])>sort(change.sco)[320*2]))]
  grp24=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*4])&((Trait20[,3])>sort(change.sco)[320*3]))]
  grp25=(1:N)[which(((Trait20[,3])<=sort(change.sco)[320*5])&((Trait20[,3])>sort(change.sco)[320*4]))]
  
  a2=a2+sum(grp21%in%grp1[,1])+sum(grp22%in%grp2[,1])+sum(grp23%in%grp3[,1])+sum(grp24%in%grp4[,1])+sum(grp25%in%grp5[,1])
  
  grp31=(1:N)[which((Trait30[,3])<=sort(change.sco)[320])]
  grp32=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*2])&((Trait30[,3])>sort(change.sco)[320]))]
  grp33=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*3])&((Trait30[,3])>sort(change.sco)[320*2]))]
  grp34=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*4])&((Trait30[,3])>sort(change.sco)[320*3]))]
  grp35=(1:N)[which(((Trait30[,3])<=sort(change.sco)[320*5])&((Trait30[,3])>sort(change.sco)[320*4]))]
  
  a3=a3+sum(grp31%in%grp1[,1])+sum(grp32%in%grp2[,1])+sum(grp33%in%grp3[,1])+sum(grp34%in%grp4[,1])+sum(grp35%in%grp5[,1])
  
  #set.seed(rep)
  #km <- kmeans(Trait10[,3], 5, nstart=5000)
  #cluster1=km$cluster
}
a1/(1600*reps) #0.19875
a2/(1600*reps) #0.2045
a3/(1600*reps) #0.198875


