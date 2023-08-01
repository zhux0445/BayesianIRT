################
#     1.1
################

setwd("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods")

setwd("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim2Concurrent")

setwd("F:/PsychMethod/Condition1ShortHardSim2Concurrent")


J=9
1.1/1.7
2.8/1.7
set.seed(1)
Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
set.seed(100)
Gamma1=runif(3,0.647,1.647) #0.9547661 0.9046725 1.1993224 
gra=matrix(0,12,5)
gra[1:6,1]= Amat1*1.7
gra[7:12,2] = Amat2*1.7
for (j in 1:3){
  gra[j,j+2]=gra[j+6,j+2]=Gamma1[j]*1.7
}

set.seed(1)
grb1<-runif(J,-3,-1)
grb2<-runif(J,-1,1)
grb3<-runif(J,1,3)


grb<-matrix(c(grb1,grb2,grb3),nrow=J)
grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
grorb
round(grorb,3)
grd=rbind(-grorb[1:6,]*1.7,-grorb[1:3,]*1.7,-grorb[7:9,]*1.7)

N1=800
N2=800
N=N1+N2
reps=50
set.seed(1)
Theta1=rmvnorm(N1*reps,c(0,0.2),matrix(c(1,0.4,0.4,1),2,2))
Theta2=rmvnorm(N2*reps,c(0.1,0.3),matrix(c(1,0.4,0.4,1),2,2))
Theta.nuisance=matrix(rnorm(N*reps*J,0,1),N*reps,3)

write.csv(Theta1,file='Condition2_trait_cohort1_shorttest_hard.csv')
write.csv(Theta2,file='Condition2_trait_cohort2_shorttest_hard.csv')
write.csv(Theta.nuisance,file='Condition2_trait_nuisance_cohort12_shorttest_hard.csv')
#quant1=quantile(c(Theta1[,2]-Theta1[,1],Theta2[,2]-Theta2[,1]), probs = seq(0, 1, 0.2), na.rm = FALSE,names = TRUE, type = 7)
#quant1
#which((Theta[,2]-Theta[,1])>(-3.2))

#con1.dat=cbind(Theta,Theta[,2]-Theta[,1],cohort1)
#con1.dat=as.data.frame(con1.dat)
#colnames(con1.dat)=c("baseline", "followup","change","cohort")
#head(con1.dat)
#newdata <- con1.dat[order(con1.dat$change),]



#items <- c(rep('2PL',3),'graded','2PL')
#dataset=simdata(Amat1,Dmat1,itemtype = items,Theta=matrix(Theta))

#cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))

#cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))


cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))

cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))

#colnames(resp.all)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
#                     "y13","y14","y15","y16","y17","y18","y19","y20","y21","y22","y23","y24")
#resp.all=as.data.frame(resp.all)
#resp.all <- resp.all[order(resp.all$rid),]

write.csv(cohort1.resp,file='sim2_cohort1_resp_shorttest_hard.csv')
write.csv(cohort2.resp,file='sim2_cohort2_resp_shorttest_hard.csv')

cohort1.resp=read.csv(file='sim2_cohort1_resp_shorttest_hard.csv')[,-1]
cohort2.resp=read.csv(file='sim2_cohort2_resp_shorttest_hard.csv')[,-1]

#cohort1.resp=read.csv(file='sim2_cohort1_resp.csv')
#cohort2.resp=read.csv(file='sim2_cohort2_resp.csv')


# 1. Concurrent calibration (provided that the model converges)
N1=N2=800
for (rep in 1:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  coh12.resp=rbind(coh1.resp,coh2.resp)
  coh12.resp=cbind(coh12.resp,c(rep(1,N1),rep(2,N2)))
  colnames(coh12.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","Phase")
  
  model.concurrent <- " %OVERALL%
                       th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	         
                       
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     
	                   
	                     th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;
  
                        %c#1%
  	                     [th1_1@0];
                         th1_1 @1;
                         [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;
                       
  	                     %c#2%
                         [th1_1*0];
                         th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;"
  
  body.model.concurrent <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid; CLASSES=c(2); KNOWNCLASS=c(Phase=1-2);",
    ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.concurrent,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2_ConcurrentTrait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh12.resp),
    rdata 	= coh12.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                               modelout = paste0("Sim2_Concurrent_shorttest_hard",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
}


# 2. Bayesian estimation (using informative priors)
for (rep in 2:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  
  model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
  
  
  body.model.Bayesian1 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian1,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2_Bayesian1Trait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                               modelout = paste0("Sim2_shorttest_Bayesian1_hard_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_",rep,".out"))$parameters$unstandardized[,3]
  var.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_",rep,".out"))$parameters$unstandardized[,4]^2
  #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
  
  model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1] (t10);
                       [y4$2] (t11);
                       [y4$3] (t12);
	                     [y5$1] (t13);
                       [y5$2] (t14);
                       [y5$3] (t15);
                       [y6$1] (t16);
                       [y6$2] (t17);
                       [y6$3] (t18);
	                     [y10$1] (t19);
                       [y10$2] (t20);
                       [y10$3] (t21);
                       [y11$1] (t22);
                       [y11$2] (t23);
                       [y11$3] (t24);
                       [y12$1] (t25);
                       [y12$2] (t26);
                       [y12$3] (t27);
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
  
  
  
  
  MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               l_4~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_5~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_6~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_7~ N(",est.Bayesian1[10]," ",var.Bayesian1[10],");
               l_8~ N(",est.Bayesian1[11]," ",var.Bayesian1[11],");
               l_9~ N(",est.Bayesian1[12]," ",var.Bayesian1[12],");
               
               t1~ N(",est.Bayesian1[34]," ",var.Bayesian1[34],");
               t2~ N(",est.Bayesian1[35]," ",var.Bayesian1[35],");
               t3~ N(",est.Bayesian1[36]," ",var.Bayesian1[36],");
               t4~ N(",est.Bayesian1[37]," ",var.Bayesian1[37],");
               t5~ N(",est.Bayesian1[38]," ",var.Bayesian1[38],");
               t6~ N(",est.Bayesian1[39]," ",var.Bayesian1[39],");
               t7~ N(",est.Bayesian1[40]," ",var.Bayesian1[40],");
               t8~ N(",est.Bayesian1[41]," ",var.Bayesian1[41],");
               t9~ N(",est.Bayesian1[42]," ",var.Bayesian1[42],");
               t10~ N(",est.Bayesian1[43]," ",var.Bayesian1[43],");
               t11~ N(",est.Bayesian1[44]," ",var.Bayesian1[44],");
               t12~ N(",est.Bayesian1[45]," ",var.Bayesian1[45],");
               t13~ N(",est.Bayesian1[46]," ",var.Bayesian1[46],");
               t14~ N(",est.Bayesian1[47]," ",var.Bayesian1[47],");
               t15~ N(",est.Bayesian1[48]," ",var.Bayesian1[48],");
               t16~ N(",est.Bayesian1[49]," ",var.Bayesian1[49],");
               t17~ N(",est.Bayesian1[50]," ",var.Bayesian1[50],");
               t18~ N(",est.Bayesian1[51]," ",var.Bayesian1[51],");
               t19~ N(",est.Bayesian1[61]," ",var.Bayesian1[61],");
               t20~ N(",est.Bayesian1[62]," ",var.Bayesian1[62],");
               t21~ N(",est.Bayesian1[63]," ",var.Bayesian1[63],");
               t22~ N(",est.Bayesian1[64]," ",var.Bayesian1[64],");
               t23~ N(",est.Bayesian1[65]," ",var.Bayesian1[65],");
               t24~ N(",est.Bayesian1[66]," ",var.Bayesian1[66],");
               t25~ N(",est.Bayesian1[67]," ",var.Bayesian1[67],");
               t26~ N(",est.Bayesian1[68]," ",var.Bayesian1[68],");
               t27~ N(",est.Bayesian1[69]," ",var.Bayesian1[69],");")
  
  
  
  
  body.model.Bayesian2 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian2,
    MODELPRIORS = MODELPRIORS21,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2_Bayesian2Trait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                               modelout = paste0("Sim2_shorttest_Bayesian2_hard_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  est.Bayesian2=readModels(paste0("sim2_shorttest_Bayesian2_hard_",rep,".out"))$parameters$unstandardized[,3]
  model.Bayesian3 <- paste0("th1_1  by  y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3]," ","y4@",est.Bayesian2[4]," 
                            y5@",est.Bayesian2[5]," ","y6@",est.Bayesian2[6],";
                            th1_2  by  y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3]," ","y10@",est.Bayesian2[10]," 
                            y11@",est.Bayesian2[11]," ","y12@",est.Bayesian2[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian2[34]," ","y7$1@",est.Bayesian2[34],"];
                       [y1$2@",est.Bayesian2[35]," ", "y7$2@",est.Bayesian2[35],"];
                       [y1$3@",est.Bayesian2[36]," ", "y7$3@",est.Bayesian2[36],"];
                       [y2$1@",est.Bayesian2[37]," ", "y8$1@",est.Bayesian2[37],"];
                       [y2$2@",est.Bayesian2[38]," ", "y8$2@",est.Bayesian2[38],"];
                       [y2$3@",est.Bayesian2[39]," ", "y8$3@",est.Bayesian2[39],"];
                       [y3$1@",est.Bayesian2[40]," ", "y9$1@",est.Bayesian2[40],"];
                       [y3$2@",est.Bayesian2[41]," ", "y9$2@",est.Bayesian2[41],"];
                       [y3$3@",est.Bayesian2[42]," ", "y9$3@",est.Bayesian2[42],"];
                       [y4$1@",est.Bayesian2[43],"];
                       [y4$2@",est.Bayesian2[44],"];
                       [y4$3@",est.Bayesian2[45],"];
                       [y5$1@",est.Bayesian2[46],"];
                       [y5$2@",est.Bayesian2[47],"];
                       [y5$3@",est.Bayesian2[48],"];
                       [y6$1@",est.Bayesian2[49],"];
                       [y6$2@",est.Bayesian2[50],"];
                       [y6$3@",est.Bayesian2[51],"];
                       [y10$1@",est.Bayesian2[61],"];
                       [y10$2@",est.Bayesian2[62],"];
                       [y10$3@",est.Bayesian2[63],"];
                       [y11$1@",est.Bayesian2[64],"];
                       [y11$2@",est.Bayesian2[65],"];
                       [y11$3@",est.Bayesian2[66],"];
                       [y12$1@",est.Bayesian2[67],"];
                       [y12$2@",est.Bayesian2[68],"];
                       [y12$3@",est.Bayesian2[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3")
  
  
  body.model.Bayesian3 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian3,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2_Bayesian3Trait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                               modelout = paste0("Sim2_shorttest_Bayesian3_hard_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
}



# 3. Ignore repeated measure, fixed item parameters 

for (rep in 2:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  
  coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
  coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
  colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
  #coh12.resp=rbind(coh1.resp,coh2.resp)
  model.Ignore1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     [th1_1@0];
                       th1_1@1;"
  
  body.model.Ignore1 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore1,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2Ignore1Trait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                               modelout = paste0("Sim2_shorttest_Ignore1_hard_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Ignore1=readModels(paste0("Sim2_shorttest_Ignore1_hard_",rep,".out"))$parameters$unstandardized[,3]
  #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
  
  model.Ignore2 <- paste0(" th1_1  by  y1@",est.Ignore1[1], " y2@",est.Ignore1[2], " y3@",est.Ignore1[3], " y4@",est.Ignore1[4],
                          " y5@",est.Ignore1[5]," y6@",est.Ignore1[6]," ;",
                          "[th1_1*0];
                       th1_1*1;
	                     [y1$1@",est.Ignore1[8],"];
                       [y1$2@",est.Ignore1[9],"];
                       [y1$3@",est.Ignore1[10],"];
	                     [y2$1@",est.Ignore1[11],"];
	                     [y2$2@",est.Ignore1[12],"];
	                     [y2$3@",est.Ignore1[13],"];
	                     [y3$1@",est.Ignore1[14],"];
	                     [y3$2@",est.Ignore1[15],"];
	                     [y3$3@",est.Ignore1[16],"];
	                     [y4$1@",est.Ignore1[17],"];
	                     [y4$2@",est.Ignore1[18],"];
	                     [y4$3@",est.Ignore1[19],"];
	                     [y5$1@",est.Ignore1[20],"];
	                     [y5$2@",est.Ignore1[21],"];
	                     [y5$3@",est.Ignore1[22],"];
	                     [y6$1@",est.Ignore1[23],"];
	                     [y6$2@",est.Ignore1[24],"];
	                     [y6$3@",est.Ignore1[25],"];")
  
  body.model.Ignore2 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore2,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2Ignore2Trait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                               modelout = paste0("Sim2_shorttest_Ignore2_hard_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  ####################
  #     Follow up
  ####################
  model.Ignore2 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3],
                          "
                          y10 y11 y12;",
                          "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore1[8],"];
                       [y7$2@",est.Ignore1[9],"];
                       [y7$3@",est.Ignore1[10],"];
	                     [y8$1@",est.Ignore1[11],"];
	                     [y8$2@",est.Ignore1[12],"];
	                     [y8$3@",est.Ignore1[13],"];
	                     [y9$1@",est.Ignore1[14],"];
	                     [y9$2@",est.Ignore1[15],"];
	                     [y9$3@",est.Ignore1[16],"];")
  
  body.model.Ignore3 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore2,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2Ignore3Trait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp.fl),
    rdata 	= coh1.resp.fl)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                               modelout = paste0("Sim2_shorttest_Ignore3_hard_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always') 
  setwd("F:/PsychMethod/Condition1ShortHardSim2Ignore")
  setwd("~/")
  for (rep in 2:reps){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
    
    coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
    coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
    colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
    
    est.Ignore2=readModels(paste0("Sim2_shorttest_Ignore3_hard_",rep,".out"))$parameters$unstandardized[,3]
    
    model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore2[1], " y8@",est.Ignore2[2], " y9@",est.Ignore1[3],"
                          y10@", est.Ignore2[4], " y11@",est.Ignore2[5], "  y12@", est.Ignore2[6], " ;",
                            "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore2[8],"];
                       [y7$2@",est.Ignore2[9],"];
                       [y7$3@",est.Ignore2[10],"];
	                     [y8$1@",est.Ignore2[11],"];
	                     [y8$2@",est.Ignore2[12],"];
	                     [y8$3@",est.Ignore2[13],"];
	                     [y9$1@",est.Ignore2[14],"];
	                     [y9$2@",est.Ignore2[15],"];
	                     [y9$3@",est.Ignore2[16],"];
                       [y10$1@",est.Ignore2[17],"];
                       [y10$2@",est.Ignore2[18],"];
                       [y10$3@",est.Ignore2[19],"];
	                     [y11$1@",est.Ignore2[20],"];
	                     [y11$2@",est.Ignore2[21],"];
	                     [y11$3@",est.Ignore2[22],"];
	                     [y12$1@",est.Ignore2[23],"];
	                     [y12$2@",est.Ignore2[24],"];
	                     [y12$3@",est.Ignore2[25],"];")
    body.model.Ignore4 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore4,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2Ignore4Trait_shorttest_hard",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp.fl),
      rdata 	= coh2.resp.fl)
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                 modelout = paste0("Sim2_shorttest_Ignore4_hard_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')  
    
  }
  
#######################
#        1.2
#######################

  setwd("~/")
  setwd("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods")
  
  setwd("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods/Condition1Sim2Concurrent")
  
  setwd("F:/PsychMethod/Condition1ShortHardSim2Concurrent")
  
  
  J=9
  1.1/1.7
  2.8/1.7
  set.seed(1)
  Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
  Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
  Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
  set.seed(100)
  Gamma1=runif(3,0.647,1.647) #0.9547661 0.9046725 1.1993224 
  gra=matrix(0,12,5)
  gra[1:6,1]= Amat1*1.7
  gra[7:12,2] = Amat2*1.7
  for (j in 1:3){
    gra[j,j+2]=gra[j+6,j+2]=Gamma1[j]*1.7
  }
  
  set.seed(1)
  grb1<-runif(J,-3,-1)
  grb2<-runif(J,-1,1)
  grb3<-runif(J,1,3)
  
  
  grb<-matrix(c(grb1,grb2,grb3),nrow=J)
  grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
  grorb
  round(grorb,3)
  grd=rbind(-grorb[1:6,]*1.7,-grorb[1:3,]*1.7,-grorb[7:9,]*1.7)
  
  N1=800
  N2=800
  N=N1+N2
  reps=50
  set.seed(1)
  Theta1=rmvnorm(N1*reps,c(0,0.05),matrix(c(1,1.04,1.04,1.37),2,2))
  Theta2=rmvnorm(N2*reps,c(0.5,0.55),matrix(c(1.12,1.15,1.15,1.3),2,2))
  Theta.nuisance=matrix(rnorm(N*reps*3,0,1),N*reps,3)
  
  write.csv(Theta1,file='Condition2_trait_cohort1_shorttest_hard_imp.csv')
  write.csv(Theta2,file='Condition2_trait_cohort2_shorttest_hard_imp.csv')
  write.csv(Theta.nuisance,file='Condition2_trait_nuisance_cohort12_shorttest_hard_imp.csv')
  
  
  
  
  #items <- c(rep('2PL',3),'graded','2PL')
  #dataset=simdata(Amat1,Dmat1,itemtype = items,Theta=matrix(Theta))
  
  #cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
  
  #cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
  
  
  cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
  
  cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
  
  #colnames(resp.all)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
  #                     "y13","y14","y15","y16","y17","y18","y19","y20","y21","y22","y23","y24")
  #resp.all=as.data.frame(resp.all)
  #resp.all <- resp.all[order(resp.all$rid),]
  
  write.csv(cohort1.resp,file='sim2_cohort1_resp_shorttest_hard_imp.csv')
  write.csv(cohort2.resp,file='sim2_cohort2_resp_shorttest_hard_imp.csv')
  
  cohort1.resp=read.csv(file='sim2_cohort1_resp_shorttest_hard_imp.csv')[,-1]
  cohort2.resp=read.csv(file='sim2_cohort2_resp_shorttest_hard_imp.csv')[,-1]
  
  #cohort1.resp=read.csv(file='sim2_cohort1_resp.csv')
  #cohort2.resp=read.csv(file='sim2_cohort2_resp.csv')
  
  
  # 1. Concurrent calibration (provided that the model converges)
  N1=N2=800
  for (rep in 1:reps){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    coh12.resp=rbind(coh1.resp,coh2.resp)
    coh12.resp=cbind(coh12.resp,c(rep(1,N1),rep(2,N2)))
    colnames(coh12.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","Phase")
    
    model.concurrent <- " %OVERALL%
                       th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	         
                       
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     
	                   
	                     th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;
  
                        %c#1%
  	                     [th1_1@0];
                         th1_1 @1;
                         [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;
                       
  	                     %c#2%
                         [th1_1*0];
                         th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;"
    
    body.model.concurrent <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid; CLASSES=c(2); KNOWNCLASS=c(Phase=1-2);",
      ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.concurrent,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2_ConcurrentTrait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh12.resp),
      rdata 	= coh12.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                                 modelout = paste0("Sim2_Concurrent_shorttest_hard_imp",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
  }
  
  
  # 2. Bayesian estimation (using informative priors)
  for (rep in 1:10){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    
    model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
    
    
    body.model.Bayesian1 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian1,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2_Bayesian1Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                 modelout = paste0("Sim2_shorttest_Bayesian1_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
    est.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_imp_",rep,".out"))$parameters$unstandardized[,3]
    var.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_imp_",rep,".out"))$parameters$unstandardized[,4]^2
    #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
    
    model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1] (t10);
                       [y4$2] (t11);
                       [y4$3] (t12);
	                     [y5$1] (t13);
                       [y5$2] (t14);
                       [y5$3] (t15);
                       [y6$1] (t16);
                       [y6$2] (t17);
                       [y6$3] (t18);
	                     [y10$1] (t19);
                       [y10$2] (t20);
                       [y10$3] (t21);
                       [y11$1] (t22);
                       [y11$2] (t23);
                       [y11$3] (t24);
                       [y12$1] (t25);
                       [y12$2] (t26);
                       [y12$3] (t27);
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
    
    
    
    
    MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               l_4~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_5~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_6~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_7~ N(",est.Bayesian1[10]," ",var.Bayesian1[10],");
               l_8~ N(",est.Bayesian1[11]," ",var.Bayesian1[11],");
               l_9~ N(",est.Bayesian1[12]," ",var.Bayesian1[12],");
               
               t1~ N(",est.Bayesian1[34]," ",var.Bayesian1[34],");
               t2~ N(",est.Bayesian1[35]," ",var.Bayesian1[35],");
               t3~ N(",est.Bayesian1[36]," ",var.Bayesian1[36],");
               t4~ N(",est.Bayesian1[37]," ",var.Bayesian1[37],");
               t5~ N(",est.Bayesian1[38]," ",var.Bayesian1[38],");
               t6~ N(",est.Bayesian1[39]," ",var.Bayesian1[39],");
               t7~ N(",est.Bayesian1[40]," ",var.Bayesian1[40],");
               t8~ N(",est.Bayesian1[41]," ",var.Bayesian1[41],");
               t9~ N(",est.Bayesian1[42]," ",var.Bayesian1[42],");
               t10~ N(",est.Bayesian1[43]," ",var.Bayesian1[43],");
               t11~ N(",est.Bayesian1[44]," ",var.Bayesian1[44],");
               t12~ N(",est.Bayesian1[45]," ",var.Bayesian1[45],");
               t13~ N(",est.Bayesian1[46]," ",var.Bayesian1[46],");
               t14~ N(",est.Bayesian1[47]," ",var.Bayesian1[47],");
               t15~ N(",est.Bayesian1[48]," ",var.Bayesian1[48],");
               t16~ N(",est.Bayesian1[49]," ",var.Bayesian1[49],");
               t17~ N(",est.Bayesian1[50]," ",var.Bayesian1[50],");
               t18~ N(",est.Bayesian1[51]," ",var.Bayesian1[51],");
               t19~ N(",est.Bayesian1[61]," ",var.Bayesian1[61],");
               t20~ N(",est.Bayesian1[62]," ",var.Bayesian1[62],");
               t21~ N(",est.Bayesian1[63]," ",var.Bayesian1[63],");
               t22~ N(",est.Bayesian1[64]," ",var.Bayesian1[64],");
               t23~ N(",est.Bayesian1[65]," ",var.Bayesian1[65],");
               t24~ N(",est.Bayesian1[66]," ",var.Bayesian1[66],");
               t25~ N(",est.Bayesian1[67]," ",var.Bayesian1[67],");
               t26~ N(",est.Bayesian1[68]," ",var.Bayesian1[68],");
               t27~ N(",est.Bayesian1[69]," ",var.Bayesian1[69],");")
    
    
    
    
    body.model.Bayesian2 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian2,
      MODELPRIORS = MODELPRIORS21,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2_Bayesian2Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp),
      rdata 	= coh2.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                 modelout = paste0("Sim2_shorttest_Bayesian2_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    est.Bayesian2=readModels(paste0("sim2_shorttest_Bayesian2_hard_imp_",rep,".out"))$parameters$unstandardized[,3]
    model.Bayesian3 <- paste0("th1_1  by  y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3]," ","y4@",est.Bayesian2[4]," 
                            y5@",est.Bayesian2[5]," ","y6@",est.Bayesian2[6],";
                            th1_2  by  y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3]," ","y10@",est.Bayesian2[10]," 
                            y11@",est.Bayesian2[11]," ","y12@",est.Bayesian2[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian2[34]," ","y7$1@",est.Bayesian2[34],"];
                       [y1$2@",est.Bayesian2[35]," ", "y7$2@",est.Bayesian2[35],"];
                       [y1$3@",est.Bayesian2[36]," ", "y7$3@",est.Bayesian2[36],"];
                       [y2$1@",est.Bayesian2[37]," ", "y8$1@",est.Bayesian2[37],"];
                       [y2$2@",est.Bayesian2[38]," ", "y8$2@",est.Bayesian2[38],"];
                       [y2$3@",est.Bayesian2[39]," ", "y8$3@",est.Bayesian2[39],"];
                       [y3$1@",est.Bayesian2[40]," ", "y9$1@",est.Bayesian2[40],"];
                       [y3$2@",est.Bayesian2[41]," ", "y9$2@",est.Bayesian2[41],"];
                       [y3$3@",est.Bayesian2[42]," ", "y9$3@",est.Bayesian2[42],"];
                       [y4$1@",est.Bayesian2[43],"];
                       [y4$2@",est.Bayesian2[44],"];
                       [y4$3@",est.Bayesian2[45],"];
                       [y5$1@",est.Bayesian2[46],"];
                       [y5$2@",est.Bayesian2[47],"];
                       [y5$3@",est.Bayesian2[48],"];
                       [y6$1@",est.Bayesian2[49],"];
                       [y6$2@",est.Bayesian2[50],"];
                       [y6$3@",est.Bayesian2[51],"];
                       [y10$1@",est.Bayesian2[61],"];
                       [y10$2@",est.Bayesian2[62],"];
                       [y10$3@",est.Bayesian2[63],"];
                       [y11$1@",est.Bayesian2[64],"];
                       [y11$2@",est.Bayesian2[65],"];
                       [y11$3@",est.Bayesian2[66],"];
                       [y12$1@",est.Bayesian2[67],"];
                       [y12$2@",est.Bayesian2[68],"];
                       [y12$3@",est.Bayesian2[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3")
    
    
    body.model.Bayesian3 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian3,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2_Bayesian3Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                 modelout = paste0("Sim2_shorttest_Bayesian3_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
    
    
  }
  
  # 2. Bayesian estimation (using informative priors + more constraint to fix the scale)
  MODELPRIORS22 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               l_4~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_5~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_6~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_7~ N(",est.Bayesian1[10]," ",var.Bayesian1[10],");
               l_8~ N(",est.Bayesian1[11]," ",var.Bayesian1[11],");
               l_9~ N(",est.Bayesian1[12]," ",var.Bayesian1[12],");
               
               t1~ N(",est.Bayesian1[34]," ",var.Bayesian1[34],");
               t2~ N(",est.Bayesian1[35]," ",var.Bayesian1[35],");
               t3~ N(",est.Bayesian1[36]," ",var.Bayesian1[36],");
               t4~ N(",est.Bayesian1[37]," ",var.Bayesian1[37],");
               t5~ N(",est.Bayesian1[38]," ",var.Bayesian1[38],");
               t6~ N(",est.Bayesian1[39]," ",var.Bayesian1[39],");
               t7~ N(",est.Bayesian1[40]," ",var.Bayesian1[40],");
               t8~ N(",est.Bayesian1[41]," ",var.Bayesian1[41],");
               t9~ N(",est.Bayesian1[42]," ",var.Bayesian1[42],");
               t10~ N(",est.Bayesian1[43]," ",var.Bayesian1[43],");
               t11~ N(",est.Bayesian1[44]," ",var.Bayesian1[44],");
               t12~ N(",est.Bayesian1[45]," ",var.Bayesian1[45],");
               t13~ N(",est.Bayesian1[46]," ",var.Bayesian1[46],");
               t14~ N(",est.Bayesian1[47]," ",var.Bayesian1[47],");
               t15~ N(",est.Bayesian1[48]," ",var.Bayesian1[48],");
               t16~ N(",est.Bayesian1[49]," ",var.Bayesian1[49],");
               t17~ N(",est.Bayesian1[50]," ",var.Bayesian1[50],");
               t18~ N(",est.Bayesian1[51]," ",var.Bayesian1[51],");
               t19~ N(",est.Bayesian1[61]," ",var.Bayesian1[61],");
               t20~ N(",est.Bayesian1[62]," ",var.Bayesian1[62],");
               t21~ N(",est.Bayesian1[63]," ",var.Bayesian1[63],");
               t22~ N(",est.Bayesian1[64]," ",var.Bayesian1[64],");
               t23~ N(",est.Bayesian1[65]," ",var.Bayesian1[65],");
               t24~ N(",est.Bayesian1[66]," ",var.Bayesian1[66],");
               t25~ N(",est.Bayesian1[67]," ",var.Bayesian1[67],");
               t26~ N(",est.Bayesian1[68]," ",var.Bayesian1[68],");
               t27~ N(",est.Bayesian1[69]," ",var.Bayesian1[69],");")
  
  
  
  
  body.model.Bayesian22 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian2,
    MODELPRIORS = MODELPRIORS22,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2_Bayesian2Trait_shorttest_hard_imp_c",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian22, 
                                               modelout = paste0("Sim2_shorttest_Bayesian2_hard_imp_c_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  est.Bayesian2=readModels(paste0("sim2_shorttest_Bayesian2_hard_imp_c_",rep,".out"))$parameters$unstandardized[,3]
  model.Bayesian3 <- paste0("th1_1  by  y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3]," ","y4@",est.Bayesian2[4]," 
                            y5@",est.Bayesian2[5]," ","y6@",est.Bayesian2[6],";
                            th1_2  by  y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3]," ","y10@",est.Bayesian2[10]," 
                            y11@",est.Bayesian2[11]," ","y12@",est.Bayesian2[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian2[34]," ","y7$1@",est.Bayesian2[34],"];
                       [y1$2@",est.Bayesian2[35]," ", "y7$2@",est.Bayesian2[35],"];
                       [y1$3@",est.Bayesian2[36]," ", "y7$3@",est.Bayesian2[36],"];
                       [y2$1@",est.Bayesian2[37]," ", "y8$1@",est.Bayesian2[37],"];
                       [y2$2@",est.Bayesian2[38]," ", "y8$2@",est.Bayesian2[38],"];
                       [y2$3@",est.Bayesian2[39]," ", "y8$3@",est.Bayesian2[39],"];
                       [y3$1@",est.Bayesian2[40]," ", "y9$1@",est.Bayesian2[40],"];
                       [y3$2@",est.Bayesian2[41]," ", "y9$2@",est.Bayesian2[41],"];
                       [y3$3@",est.Bayesian2[42]," ", "y9$3@",est.Bayesian2[42],"];
                       [y4$1@",est.Bayesian2[43],"];
                       [y4$2@",est.Bayesian2[44],"];
                       [y4$3@",est.Bayesian2[45],"];
                       [y5$1@",est.Bayesian2[46],"];
                       [y5$2@",est.Bayesian2[47],"];
                       [y5$3@",est.Bayesian2[48],"];
                       [y6$1@",est.Bayesian2[49],"];
                       [y6$2@",est.Bayesian2[50],"];
                       [y6$3@",est.Bayesian2[51],"];
                       [y10$1@",est.Bayesian2[61],"];
                       [y10$2@",est.Bayesian2[62],"];
                       [y10$3@",est.Bayesian2[63],"];
                       [y11$1@",est.Bayesian2[64],"];
                       [y11$2@",est.Bayesian2[65],"];
                       [y11$3@",est.Bayesian2[66],"];
                       [y12$1@",est.Bayesian2[67],"];
                       [y12$2@",est.Bayesian2[68],"];
                       [y12$3@",est.Bayesian2[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3")
  
  
  body.model.Bayesian3 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian3,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim2_Bayesian3Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                               modelout = paste0("Sim2_shorttest_Bayesian3_hard_imp_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  # 2. Bayesian estimation (using informative priors + more constraint to fix the scale)
  for (rep in 2:reps){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    
    model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
    
    
    body.model.Bayesian1 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian1,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2_Bayesian1Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                 modelout = paste0("Sim2_shorttest_Bayesian1_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
    est.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_imp_",rep,".out"))$parameters$unstandardized[,3]
    var.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_imp_",rep,".out"))$parameters$unstandardized[,4]^2
    #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
    
    model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1;
	                     th2_2  by  y2@1 y8@1;
	                     th2_3  by  y3@1 y9@1;
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1 (n1);
                       th2_2 (n2);
                       th2_3 (n3);
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1] (t10);
                       [y4$2] (t11);
                       [y4$3] (t12);
	                     [y5$1] (t13);
                       [y5$2] (t14);
                       [y5$3] (t15);
                       [y6$1] (t16);
                       [y6$2] (t17);
                       [y6$3] (t18);
	                     [y10$1] (t19);
                       [y10$2] (t20);
                       [y10$3] (t21);
                       [y11$1] (t22);
                       [y11$2] (t23);
                       [y11$3] (t24);
                       [y12$1] (t25);
                       [y12$2] (t26);
                       [y12$3] (t27);
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
    
    
    
    
    MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               l_4~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_5~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_6~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_7~ N(",est.Bayesian1[10]," ",var.Bayesian1[10],");
               l_8~ N(",est.Bayesian1[11]," ",var.Bayesian1[11],");
               l_9~ N(",est.Bayesian1[12]," ",var.Bayesian1[12],");
               
               n1~ N(",est.Bayesian1[72]," ",var.Bayesian1[72],");
               n2~ N(",est.Bayesian1[73]," ",var.Bayesian1[73],");
               n3~ N(",est.Bayesian1[74]," ",var.Bayesian1[74],");
               
               t1~ N(",est.Bayesian1[34]," ",var.Bayesian1[34],");
               t2~ N(",est.Bayesian1[35]," ",var.Bayesian1[35],");
               t3~ N(",est.Bayesian1[36]," ",var.Bayesian1[36],");
               t4~ N(",est.Bayesian1[37]," ",var.Bayesian1[37],");
               t5~ N(",est.Bayesian1[38]," ",var.Bayesian1[38],");
               t6~ N(",est.Bayesian1[39]," ",var.Bayesian1[39],");
               t7~ N(",est.Bayesian1[40]," ",var.Bayesian1[40],");
               t8~ N(",est.Bayesian1[41]," ",var.Bayesian1[41],");
               t9~ N(",est.Bayesian1[42]," ",var.Bayesian1[42],");
               t10~ N(",est.Bayesian1[43]," ",var.Bayesian1[43],");
               t11~ N(",est.Bayesian1[44]," ",var.Bayesian1[44],");
               t12~ N(",est.Bayesian1[45]," ",var.Bayesian1[45],");
               t13~ N(",est.Bayesian1[46]," ",var.Bayesian1[46],");
               t14~ N(",est.Bayesian1[47]," ",var.Bayesian1[47],");
               t15~ N(",est.Bayesian1[48]," ",var.Bayesian1[48],");
               t16~ N(",est.Bayesian1[49]," ",var.Bayesian1[49],");
               t17~ N(",est.Bayesian1[50]," ",var.Bayesian1[50],");
               t18~ N(",est.Bayesian1[51]," ",var.Bayesian1[51],");
               t19~ N(",est.Bayesian1[61]," ",var.Bayesian1[61],");
               t20~ N(",est.Bayesian1[62]," ",var.Bayesian1[62],");
               t21~ N(",est.Bayesian1[63]," ",var.Bayesian1[63],");
               t22~ N(",est.Bayesian1[64]," ",var.Bayesian1[64],");
               t23~ N(",est.Bayesian1[65]," ",var.Bayesian1[65],");
               t24~ N(",est.Bayesian1[66]," ",var.Bayesian1[66],");
               t25~ N(",est.Bayesian1[67]," ",var.Bayesian1[67],");
               t26~ N(",est.Bayesian1[68]," ",var.Bayesian1[68],");
               t27~ N(",est.Bayesian1[69]," ",var.Bayesian1[69],");")
    
    
    
    
    body.model.Bayesian2 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian2,
      MODELPRIORS = MODELPRIORS21,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2_Bayesian2Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp),
      rdata 	= coh2.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                 modelout = paste0("Sim2_shorttest_Bayesian2_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    est.Bayesian2=readModels(paste0("sim2_shorttest_Bayesian2_hard_imp_",rep,".out"))$parameters$unstandardized[,3]
    model.Bayesian3 <- paste0("th1_1  by  y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3]," ","y4@",est.Bayesian2[4]," 
                            y5@",est.Bayesian2[5]," ","y6@",est.Bayesian2[6],";
                            th1_2  by  y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3]," ","y10@",est.Bayesian2[10]," 
                            y11@",est.Bayesian2[11]," ","y12@",est.Bayesian2[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian2[34]," ","y7$1@",est.Bayesian2[34],"];
                       [y1$2@",est.Bayesian2[35]," ", "y7$2@",est.Bayesian2[35],"];
                       [y1$3@",est.Bayesian2[36]," ", "y7$3@",est.Bayesian2[36],"];
                       [y2$1@",est.Bayesian2[37]," ", "y8$1@",est.Bayesian2[37],"];
                       [y2$2@",est.Bayesian2[38]," ", "y8$2@",est.Bayesian2[38],"];
                       [y2$3@",est.Bayesian2[39]," ", "y8$3@",est.Bayesian2[39],"];
                       [y3$1@",est.Bayesian2[40]," ", "y9$1@",est.Bayesian2[40],"];
                       [y3$2@",est.Bayesian2[41]," ", "y9$2@",est.Bayesian2[41],"];
                       [y3$3@",est.Bayesian2[42]," ", "y9$3@",est.Bayesian2[42],"];
                       [y4$1@",est.Bayesian2[43],"];
                       [y4$2@",est.Bayesian2[44],"];
                       [y4$3@",est.Bayesian2[45],"];
                       [y5$1@",est.Bayesian2[46],"];
                       [y5$2@",est.Bayesian2[47],"];
                       [y5$3@",est.Bayesian2[48],"];
                       [y6$1@",est.Bayesian2[49],"];
                       [y6$2@",est.Bayesian2[50],"];
                       [y6$3@",est.Bayesian2[51],"];
                       [y10$1@",est.Bayesian2[61],"];
                       [y10$2@",est.Bayesian2[62],"];
                       [y10$3@",est.Bayesian2[63],"];
                       [y11$1@",est.Bayesian2[64],"];
                       [y11$2@",est.Bayesian2[65],"];
                       [y11$3@",est.Bayesian2[66],"];
                       [y12$1@",est.Bayesian2[67],"];
                       [y12$2@",est.Bayesian2[68],"];
                       [y12$3@",est.Bayesian2[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3")
    
    
    body.model.Bayesian3 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian3,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2_Bayesian3Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                 modelout = paste0("Sim2_shorttest_Bayesian3_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
  }
  # 3. Ignore repeated measure, fixed item parameters 
  
  for (rep in 1:10){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
    
    coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
    coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
    colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
    #coh12.resp=rbind(coh1.resp,coh2.resp)
    model.Ignore1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     [th1_1@0];
                       th1_1@1;"
    
    body.model.Ignore1 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore1,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2Ignore1Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                                 modelout = paste0("Sim2_shorttest_Ignore1_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
    est.Ignore1=readModels(paste0("Sim2_shorttest_Ignore1_hard_imp_",rep,".out"))$parameters$unstandardized[,3]
    #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
    
    model.Ignore2 <- paste0(" th1_1  by  y1@",est.Ignore1[1], " y2@",est.Ignore1[2], " y3@",est.Ignore1[3], " y4@",est.Ignore1[4],
                            " y5@",est.Ignore1[5]," y6@",est.Ignore1[6]," ;",
                            "[th1_1*0];
                       th1_1*1;
	                     [y1$1@",est.Ignore1[8],"];
                       [y1$2@",est.Ignore1[9],"];
                       [y1$3@",est.Ignore1[10],"];
	                     [y2$1@",est.Ignore1[11],"];
	                     [y2$2@",est.Ignore1[12],"];
	                     [y2$3@",est.Ignore1[13],"];
	                     [y3$1@",est.Ignore1[14],"];
	                     [y3$2@",est.Ignore1[15],"];
	                     [y3$3@",est.Ignore1[16],"];
	                     [y4$1@",est.Ignore1[17],"];
	                     [y4$2@",est.Ignore1[18],"];
	                     [y4$3@",est.Ignore1[19],"];
	                     [y5$1@",est.Ignore1[20],"];
	                     [y5$2@",est.Ignore1[21],"];
	                     [y5$3@",est.Ignore1[22],"];
	                     [y6$1@",est.Ignore1[23],"];
	                     [y6$2@",est.Ignore1[24],"];
	                     [y6$3@",est.Ignore1[25],"];")
    
    body.model.Ignore2 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore2,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2Ignore2Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp),
      rdata 	= coh2.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                                 modelout = paste0("Sim2_shorttest_Ignore2_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    ####################
    #     Follow up
    ####################
    model.Ignore2 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3],
                            "
                          y10 y11 y12;",
                            "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore1[8],"];
                       [y7$2@",est.Ignore1[9],"];
                       [y7$3@",est.Ignore1[10],"];
	                     [y8$1@",est.Ignore1[11],"];
	                     [y8$2@",est.Ignore1[12],"];
	                     [y8$3@",est.Ignore1[13],"];
	                     [y9$1@",est.Ignore1[14],"];
	                     [y9$2@",est.Ignore1[15],"];
	                     [y9$3@",est.Ignore1[16],"];")
    
    body.model.Ignore3 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore2,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim2Ignore3Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp.fl),
      rdata 	= coh1.resp.fl)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                                 modelout = paste0("Sim2_shorttest_Ignore3_hard_imp_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')  
    for (rep in 1:10){
      est.Ignore2=readModels(paste0("Sim2_shorttest_Ignore3_hard_imp_",rep,".out"))$parameters$unstandardized[,3]
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      
      coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
      coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
      colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore2[1], " y8@",est.Ignore2[2], " y9@",est.Ignore1[3],"
                          y10@", est.Ignore2[4], " y11@",est.Ignore2[5], "  y12@", est.Ignore2[6], " ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore2[8],"];
                       [y7$2@",est.Ignore2[9],"];
                       [y7$3@",est.Ignore2[10],"];
	                     [y8$1@",est.Ignore2[11],"];
	                     [y8$2@",est.Ignore2[12],"];
	                     [y8$3@",est.Ignore2[13],"];
	                     [y9$1@",est.Ignore2[14],"];
	                     [y9$2@",est.Ignore2[15],"];
	                     [y9$3@",est.Ignore2[16],"];
                       [y10$1@",est.Ignore2[17],"];
                       [y10$2@",est.Ignore2[18],"];
                       [y10$3@",est.Ignore2[19],"];
	                     [y11$1@",est.Ignore2[20],"];
	                     [y11$2@",est.Ignore2[21],"];
	                     [y11$3@",est.Ignore2[22],"];
	                     [y12$1@",est.Ignore2[23],"];
	                     [y12$2@",est.Ignore2[24],"];
	                     [y12$3@",est.Ignore2[25],"];")
      body.model.Ignore4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore4Trait_shorttest_hard_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp.fl),
        rdata 	= coh2.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                   modelout = paste0("Sim2_shorttest_Ignore4_hard_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
    }
    

##########################
#         1.3 
##########################

    setwd("~/")
    setwd("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods")
    
    J=9
    1.1/1.7
    2.8/1.7
    set.seed(1)
    Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
    Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
    Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
    set.seed(100)
    Gamma1=runif(6,0.647,1.647) #0.9547661 0.9046725 1.1993224 0.7033832 1.1155493 1.1307707
    Gamma2=c(Gamma1[1:3],runif(3,0.647,1.647)) #0.9547661 0.9046725 1.1993224 1.4594026 1.0173205 1.1935586
    gra1=gra2=matrix(0,12,8)
    gra1[1:6,1]= Amat1*1.7
    gra1[7:12,2] = Amat1*1.7
    for (j in 1:6){
      gra1[j,j+2]=gra1[j+6,j+2]=Gamma1[j]*1.7
    }
    gra2[1:6,1]= Amat2*1.7
    gra2[7:12,2] = Amat2*1.7
    for (j in 1:6){
      gra2[j,j+2]=gra2[j+6,j+2]=Gamma2[j]*1.7
    }
    set.seed(1)
    grb1<-runif(J,-3,-1)
    grb2<-runif(J,-1,1)
    grb3<-runif(J,1,3)
    
    
    grb<-matrix(c(grb1,grb2,grb3),nrow=J)
    grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
    grorb
    round(grorb,3)
    grd1=rbind(-grorb[1:6,]*1.7,-grorb[1:6,]*1.7)
    grd2=rbind(-grorb[1:3,]*1.7,-grorb[7:9,]*1.7,-grorb[1:3,]*1.7,-grorb[7:9,]*1.7)
    
    N1=800
    N2=800
    N=N1+N2
    reps=50
    set.seed(1)
    Theta1=rmvnorm(N1*reps,c(0,0.2),matrix(c(1,0.4,0.4,1),2,2))
    Theta2=rmvnorm(N2*reps,c(0.1,0.3),matrix(c(1,0.4,0.4,1),2,2))
    Theta.nuisance=matrix(rnorm(N*reps*6,0,1),N*reps,6)
    
    write.csv(Theta1,file='Condition2_trait_cohort1_ADNILan.csv')
    write.csv(Theta2,file='Condition2_trait_cohort2_ADNILan.csv')
    write.csv(Theta.nuisance,file='Condition2_trait_nuisance_cohort12_ADNILan.csv')
    #quant1=quantile(c(Theta1[,2]-Theta1[,1],Theta2[,2]-Theta2[,1]), probs = seq(0, 1, 0.2), na.rm = FALSE,names = TRUE, type = 7)
    #quant1
    #which((Theta[,2]-Theta[,1])>(-3.2))
    
    #con1.dat=cbind(Theta,Theta[,2]-Theta[,1],cohort1)
    #con1.dat=as.data.frame(con1.dat)
    #colnames(con1.dat)=c("baseline", "followup","change","cohort")
    #head(con1.dat)
    #newdata <- con1.dat[order(con1.dat$change),]
    
    
    
    #items <- c(rep('2PL',3),'graded','2PL')
    #dataset=simdata(Amat1,Dmat1,itemtype = items,Theta=matrix(Theta))
    
    #cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
    
    #cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
    
    
    cohort1.resp <- simdata(gra1,grd1,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
    
    cohort2.resp <- simdata(gra2,grd2,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
    
    #colnames(resp.all)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
    #                     "y13","y14","y15","y16","y17","y18","y19","y20","y21","y22","y23","y24")
    #resp.all=as.data.frame(resp.all)
    #resp.all <- resp.all[order(resp.all$rid),]
    
    write.csv(cohort1.resp,file='sim2_cohort1_resp_ADNILan.csv')
    write.csv(cohort2.resp,file='sim2_cohort2_resp_ADNILan.csv')
    
    cohort1.resp=read.csv(file='sim2_cohort1_resp_ADNILan.csv')[,-1]
    cohort2.resp=read.csv(file='sim2_cohort2_resp_ADNILan.csv')[,-1]
    
    #cohort1.resp=read.csv(file='sim2_cohort1_resp.csv')
    #cohort2.resp=read.csv(file='sim2_cohort2_resp.csv')
    
    setwd("F:/PsychMethod/Condition1Sim2ConcurrentADNILan")
    setwd("~/")
    # 1. Concurrent calibration (provided that the model converges)
    N1=N2=800
    for (rep in 1:20){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6],NA,NA,NA,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12],NA,NA,NA)
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:3],NA,NA,NA,cohort2.resp[((rep-1)*N2+1):(rep*N2),4:9],NA,NA,NA,cohort2.resp[((rep-1)*N2+1):(rep*N2),10:12])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      coh12.resp=rbind(coh1.resp,coh2.resp)
      coh12.resp=cbind(coh12.resp,c(rep(1,N1),rep(2,N2)))
      colnames(coh12.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
                             "y13","y14","y15","y16","y17","y18","Phase")
      
      model.concurrent <- " %OVERALL%
                       th1_1  by  y1-y9* (l_1-l_9);
	                     th1_2  by  y10-y18* (l_1-l_9); 
	                     th2_1  by  y1@1 y10@1 (n1);
	                     th2_2  by  y2@1 y11@1 (n2);
	                     th2_3  by  y3@1 y12@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	                     th2_7  by  y7@1 y16@1 (n7);
	                     th2_8  by  y8@1 y17@1 (n8);
	                     th2_9  by  y9@1 y18@1 (n9);
	
                       [th2_1-th2_9@0];
                       th2_1-th2_9*1;
                       [y1$1 y10$1] (t1);
                       [y1$2 y10$2] (t2);
                       [y1$3 y10$3] (t3);
                       [y2$1 y11$1] (t4);
                       [y2$2 y11$2] (t5);
                       [y2$3 y11$3] (t6);
                       
	                     [y3$1 y12$1] (t7);
	                     [y3$2 y12$2] (t8);
	                     [y3$3 y12$3] (t9);
	                     [y4$1 y13$1] (t10);
	                     [y4$2 y13$2] (t11);
	                     [y4$3 y13$3] (t12);
	                     
	                     [y5$1 y14$1] (t13);
	                     [y5$2 y14$2] (t14);
	                     [y5$3 y14$3] (t15);
	                     [y6$1 y15$1] (t16);
	                     [y6$2 y15$2] (t17);
	                     [y6$3 y15$3] (t18);
	                     
	                     [y7$1 y16$1] (t19);
	                     [y7$2 y16$2] (t20);
	                     [y7$3 y16$3] (t21);
	                     [y8$1 y17$1] (t22);
	                     [y8$2 y17$2] (t23);
	                     [y8$3 y17$3] (t24);
	                     
	                     [y9$1 y18$1] (t25);
	                     [y9$2 y18$2] (t26);
	                     [y9$3 y18$3] (t27);
	                     
	                     th1_1 WITH th2_1-th2_9@0;
                       th1_2 WITH th2_1-th2_9@0;
                       th2_1 WITH th2_2-th2_9@0;
                       th2_2 WITH th2_3-th2_9@0;
                       th2_3 WITH th2_4-th2_9@0;
                       th2_4 WITH th2_5-th2_9@0;
                       th2_5 WITH th2_6-th2_9@0;
                       th2_6 WITH th2_7-th2_9@0;
                       th2_7 WITH th2_8-th2_9@0;
                       th2_8 WITH th2_9@0;
                       
                       %c#1%

                         [th1_1@0];
                         th1_1@1;
                         [th1_2*0];
                         th1_2*1;
                         [th2_1-th2_9@0];
                         th2_1-th2_9*1;
                         th1_1 WITH th1_2;

                         %c#2%

                         [th1_1*0];
                         th1_1*1;
                         [th1_2*0];
                         th1_2*1;
                         [th2_1-th2_9@0];
                         th2_1-th2_9*1;
                         th1_1 WITH th1_2;"
      
      body.model.concurrent <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;CLASSES=c(2); KNOWNCLASS=c(Phase=1-2);",
        ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.concurrent,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_ConcurrentTrait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh12.resp),
        rdata 	= coh12.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                                   modelout = paste0("Sim2_Concurrent_ADNILan",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
    }
    
    setwd("F:/PsychMethod/Condition1Sim2BayesianADNILan")
    setwd("~/")
    # 2. Bayesian estimation (using informative priors)
    for (rep in 1:20){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      
      model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_6); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y10@1 (n4);
	                     th2_5  by  y5@1 y11@1 (n5);
	                     th2_6  by  y6@1 y12@1 (n6);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1 y10$1] (t10);
                       [y4$2 y10$2] (t11);
                       [y4$3 y10$3] (t12);
	                     [y5$1 y11$1] (t13);
	                     [y5$2 y11$2] (t14);
	                     [y5$3 y11$3] (t15);
	                     [y6$1 y12$1] (t16);
	                     [y6$2 y12$2] (t17);
	                     [y6$3 y12$3] (t18);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
      
      
      body.model.Bayesian1 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Bayesian1,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian1Trait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                   modelout = paste0("Sim2_Bayesian1_ADNILan_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      
      
      est.Bayesian1=readModels(paste0("sim2_bayesian1_ADNILan_",rep,".out"))$parameters$unstandardized[,3]
      var.Bayesian1=readModels(paste0("sim2_bayesian1_ADNILan_",rep,".out"))$parameters$unstandardized[,4]^2
      #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
      
      model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_6); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y10@1 (n4);
	                     th2_5  by  y5@1 y11@1 (n5);
	                     th2_6  by  y6@1 y12@1 (n6);
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1 y10$1] (t10);
                       [y4$2 y10$2] (t11);
                       [y4$3 y10$3] (t12);
	                     [y5$1 y11$1] (t13);
	                     [y5$2 y11$2] (t14);
	                     [y5$3 y11$3] (t15);
	                     [y6$1 y12$1] (t16);
	                     [y6$2 y12$2] (t17);
	                     [y6$3 y12$3] (t18);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
      
      
      
      
      MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               
               
               t1~ N(",est.Bayesian1[61]," ",var.Bayesian1[61],");
               t2~ N(",est.Bayesian1[62]," ",var.Bayesian1[62],");
               t3~ N(",est.Bayesian1[63]," ",var.Bayesian1[63],");
               t4~ N(",est.Bayesian1[64]," ",var.Bayesian1[64],");
               t5~ N(",est.Bayesian1[65]," ",var.Bayesian1[65],");
               t6~ N(",est.Bayesian1[66]," ",var.Bayesian1[66],");
               t7~ N(",est.Bayesian1[67]," ",var.Bayesian1[67],");
               t8~ N(",est.Bayesian1[68]," ",var.Bayesian1[68],");
               t9~ N(",est.Bayesian1[69]," ",var.Bayesian1[69],");")
      
      
      
      
      body.model.Bayesian2 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian2,
        MODELPRIORS = MODELPRIORS21,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian2Trait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                   modelout = paste0("Sim2_Bayesian2_ADNILan_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      est.Bayesian2=readModels(paste0("sim2_Bayesian2_ADNILan_",rep,".out"))$parameters$unstandardized[,3]
      model.Bayesian3 <- paste0("th1_1  by  y4* y5 y6(l_1-l_3) 
      y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3],";
                            th1_2  by  y10* y11 y12(l_1-l_3) 
                            y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y10@1 (n4);
	                     th2_5  by  y5@1 y11@1 (n5);
	                     th2_6  by  y6@1 y12@1 (n6);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       
                       [y1$1@",est.Bayesian2[61]," ","y7$1@",est.Bayesian2[61],"];
                       [y1$2@",est.Bayesian2[62]," ", "y7$2@",est.Bayesian2[62],"];
                       [y1$3@",est.Bayesian2[63]," ", "y7$3@",est.Bayesian2[63],"];
                       [y2$1@",est.Bayesian2[64]," ", "y8$1@",est.Bayesian2[64],"];
                       [y2$2@",est.Bayesian2[65]," ", "y8$2@",est.Bayesian2[65],"];
                       [y2$3@",est.Bayesian2[66]," ", "y8$3@",est.Bayesian2[66],"];
                       [y3$1@",est.Bayesian2[67]," ", "y9$1@",est.Bayesian2[67],"];
                       [y3$2@",est.Bayesian2[68]," ", "y9$2@",est.Bayesian2[68],"];
                       [y3$3@",est.Bayesian2[69]," ", "y9$3@",est.Bayesian2[69],"];
                       [y4$1 y10$1] (t10);
                       [y4$2 y10$2] (t11);
                       [y4$3 y10$3] (t12);
	                     [y5$1 y11$1] (t13);
	                     [y5$2 y11$2] (t14);
	                     [y5$3 y11$3] (t15);
	                     [y6$1 y12$1] (t16);
	                     [y6$2 y12$2] (t17);
	                     [y6$3 y12$3] (t18);
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
      
      
      body.model.Bayesian3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian3Trait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                   modelout = paste0("Sim2_Bayesian3_ADNILan_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
    }
    
    
    
    # 3. Ignore repeated measure, fixed item parameters 
    setwd("F:/PsychMethod/Condition1Sim2IgnoreADNILan")
    setwd("~/")
    for (rep in 1:20){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      
      coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
      coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
      colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      #coh12.resp=rbind(coh1.resp,coh2.resp)
      model.Ignore1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     [th1_1@0];
                       th1_1@1;"
      
      body.model.Ignore1 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore1,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore1Trait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                                   modelout = paste0("Sim2_Ignore1_ADNILan_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      est.Ignore1=readModels(paste0("Sim2_Ignore1_ADNILan_",rep,".out"))$parameters$unstandardized[,3]
      #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
      
      model.Ignore2 <- paste0(" th1_1  by  y1@",est.Ignore1[1], " y2@",est.Ignore1[2], " y3@",est.Ignore1[3], " y4",
                              " y5"," y6"," ;",
                              "[th1_1*0];
                       th1_1*1;
	                     [y1$1@",est.Ignore1[8],"];
                       [y1$2@",est.Ignore1[9],"];
                       [y1$3@",est.Ignore1[10],"];
	                     [y2$1@",est.Ignore1[11],"];
	                     [y2$2@",est.Ignore1[12],"];
	                     [y2$3@",est.Ignore1[13],"];
	                     [y3$1@",est.Ignore1[14],"];
	                     [y3$2@",est.Ignore1[15],"];
	                     [y3$3@",est.Ignore1[16],"];")
      
      body.model.Ignore2 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore2,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore2Trait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                                   modelout = paste0("Sim2_Ignore2_ADNILan_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      est.Ignore2=readModels(paste0("Sim2_Ignore2_ADNILan_",rep,".out"))$parameters$unstandardized[,3]
      
      ####################
      #     Follow up
      ####################
      model.Ignore3 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3], " y10@",est.Ignore1[4],
                              " y11@",est.Ignore1[5]," y12@",est.Ignore1[6]," ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore1[8],"];
                       [y7$2@",est.Ignore1[9],"];
                       [y7$3@",est.Ignore1[10],"];
	                     [y8$1@",est.Ignore1[11],"];
	                     [y8$2@",est.Ignore1[12],"];
	                     [y8$3@",est.Ignore1[13],"];
	                     [y9$1@",est.Ignore1[14],"];
	                     [y9$2@",est.Ignore1[15],"];
	                     [y9$3@",est.Ignore1[16],"];
	                     [y10$1@",est.Ignore1[17],"];
	                     [y10$2@",est.Ignore1[18],"];
	                     [y10$3@",est.Ignore1[19],"];
	                     [y11$1@",est.Ignore1[20],"];
	                     [y11$2@",est.Ignore1[21],"];
	                     [y11$3@",est.Ignore1[22],"];
	                     [y12$1@",est.Ignore1[23],"];
	                     [y12$2@",est.Ignore1[24],"];
	                     [y12$3@",est.Ignore1[25],"];")
      
      body.model.Ignore3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore3Trait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp.fl),
        rdata 	= coh1.resp.fl)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                                   modelout = paste0("Sim2_Ignore3_ADNILan_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
      model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore2[1], " y8@",est.Ignore2[2], " y9@",est.Ignore2[3], " y10@",est.Ignore2[4],
                              " y11@",est.Ignore2[5]," y12@",est.Ignore2[6]," ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore2[8],"];
                       [y7$2@",est.Ignore2[9],"];
                       [y7$3@",est.Ignore2[10],"];
	                     [y8$1@",est.Ignore2[11],"];
	                     [y8$2@",est.Ignore2[12],"];
	                     [y8$3@",est.Ignore2[13],"];
	                     [y9$1@",est.Ignore2[14],"];
	                     [y9$2@",est.Ignore2[15],"];
	                     [y9$3@",est.Ignore2[16],"];
	                     [y10$1@",est.Ignore2[17],"];
	                     [y10$2@",est.Ignore2[18],"];
	                     [y10$3@",est.Ignore2[19],"];
	                     [y11$1@",est.Ignore2[20],"];
	                     [y11$2@",est.Ignore2[21],"];
	                     [y11$3@",est.Ignore2[22],"];
	                     [y12$1@",est.Ignore2[23],"];
	                     [y12$2@",est.Ignore2[24],"];
	                     [y12$3@",est.Ignore2[25],"];")
      
      body.model.Ignore4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore4Trait_ADNILan",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp.fl),
        rdata 	= coh2.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                   modelout = paste0("Sim2_Ignore4_ADNILan_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
    }
    
    
##############################
#           1.4
##############################

    setwd("~/")
    setwd("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods")
    
    J=9
    1.1/1.7
    2.8/1.7
    set.seed(1)
    Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
    Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
    Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
    set.seed(100)
    Gamma1=runif(6,0.647,1.647) #0.9547661 0.9046725 1.1993224 0.7033832 1.1155493 1.1307707
    Gamma2=c(Gamma1[1:3],runif(3,0.647,1.647)) #0.9547661 0.9046725 1.1993224 1.4594026 1.0173205 1.1935586
    gra1=gra2=matrix(0,12,8)
    gra1[1:6,1]= Amat1*1.7
    gra1[7:12,2] = Amat1*1.7
    for (j in 1:6){
      gra1[j,j+2]=gra1[j+6,j+2]=Gamma1[j]*1.7
    }
    gra2[1:6,1]= Amat2*1.7
    gra2[7:12,2] = Amat2*1.7
    for (j in 1:6){
      gra2[j,j+2]=gra2[j+6,j+2]=Gamma2[j]*1.7
    }
    set.seed(1)
    grb1<-runif(J,-3,-1)
    grb2<-runif(J,-1,1)
    grb3<-runif(J,1,3)
    
    
    grb<-matrix(c(grb1,grb2,grb3),nrow=J)
    grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
    grorb
    round(grorb,3)
    grd1=rbind(-grorb[1:6,]*1.7,-grorb[1:6,]*1.7)
    grd2=rbind(-grorb[1:3,]*1.7,-grorb[7:9,]*1.7,-grorb[1:3,]*1.7,-grorb[7:9,]*1.7)
    
    N1=800
    N2=800
    N=N1+N2
    reps=50
    set.seed(1)
    Theta1=rmvnorm(N1*reps,c(0,0.2),matrix(c(1,1.04,1.04,1.37),2,2))
    Theta2=rmvnorm(N2*reps,c(0.5,0.55),matrix(c(1.12,1.15,1.15,1.3),2,2))
    Theta.nuisance=matrix(rnorm(N*reps*6,0,1),N*reps,6)
    
    write.csv(Theta1,file='Condition2_trait_cohort1_ADNILan_imp.csv')
    write.csv(Theta2,file='Condition2_trait_cohort2_ADNILan_imp.csv')
    write.csv(Theta.nuisance,file='Condition2_trait_nuisance_cohort12_ADNILan_imp.csv')
    #quant1=quantile(c(Theta1[,2]-Theta1[,1],Theta2[,2]-Theta2[,1]), probs = seq(0, 1, 0.2), na.rm = FALSE,names = TRUE, type = 7)
    #quant1
    #which((Theta[,2]-Theta[,1])>(-3.2))
    
    #con1.dat=cbind(Theta,Theta[,2]-Theta[,1],cohort1)
    #con1.dat=as.data.frame(con1.dat)
    #colnames(con1.dat)=c("baseline", "followup","change","cohort")
    #head(con1.dat)
    #newdata <- con1.dat[order(con1.dat$change),]
    
    
    
    #items <- c(rep('2PL',3),'graded','2PL')
    #dataset=simdata(Amat1,Dmat1,itemtype = items,Theta=matrix(Theta))
    
    #cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
    
    #cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
    
    
    cohort1.resp <- simdata(gra1,grd1,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
    
    cohort2.resp <- simdata(gra2,grd2,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
    
    write.csv(cohort1.resp,file='sim2_cohort1_resp_ADNILan_imp.csv')
    write.csv(cohort2.resp,file='sim2_cohort2_resp_ADNILan_imp.csv')
    
    cohort1.resp=read.csv(file='sim2_cohort1_resp_ADNILan_imp.csv')[,-1]
    cohort2.resp=read.csv(file='sim2_cohort2_resp_ADNILan_imp.csv')[,-1]
    
    setwd("F:/PsychMethod/Condition1Sim2BayesianADNILan")
    setwd("~/")
    # 2. Bayesian estimation (using informative priors)
    for (rep in 1:10){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      
      model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_6); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y10@1 (n4);
	                     th2_5  by  y5@1 y11@1 (n5);
	                     th2_6  by  y6@1 y12@1 (n6);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1 y10$1] (t10);
                       [y4$2 y10$2] (t11);
                       [y4$3 y10$3] (t12);
	                     [y5$1 y11$1] (t13);
	                     [y5$2 y11$2] (t14);
	                     [y5$3 y11$3] (t15);
	                     [y6$1 y12$1] (t16);
	                     [y6$2 y12$2] (t17);
	                     [y6$3 y12$3] (t18);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
      
      
      body.model.Bayesian1 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Bayesian1,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian1Trait_ADNILan_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                   modelout = paste0("Sim2_Bayesian1_ADNILan_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      
      
      est.Bayesian1=readModels(paste0("sim2_bayesian1_ADNILan_imp_",rep,".out"))$parameters$unstandardized[,3]
      var.Bayesian1=readModels(paste0("sim2_bayesian1_ADNILan_imp_",rep,".out"))$parameters$unstandardized[,4]^2
      #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
      
      model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_6); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y10@1 (n4);
	                     th2_5  by  y5@1 y11@1 (n5);
	                     th2_6  by  y6@1 y12@1 (n6);
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1 y10$1] (t10);
                       [y4$2 y10$2] (t11);
                       [y4$3 y10$3] (t12);
	                     [y5$1 y11$1] (t13);
	                     [y5$2 y11$2] (t14);
	                     [y5$3 y11$3] (t15);
	                     [y6$1 y12$1] (t16);
	                     [y6$2 y12$2] (t17);
	                     [y6$3 y12$3] (t18);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
      
      
      
      
      MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               
               
               t1~ N(",est.Bayesian1[61]," ",var.Bayesian1[61],");
               t2~ N(",est.Bayesian1[62]," ",var.Bayesian1[62],");
               t3~ N(",est.Bayesian1[63]," ",var.Bayesian1[63],");
               t4~ N(",est.Bayesian1[64]," ",var.Bayesian1[64],");
               t5~ N(",est.Bayesian1[65]," ",var.Bayesian1[65],");
               t6~ N(",est.Bayesian1[66]," ",var.Bayesian1[66],");
               t7~ N(",est.Bayesian1[67]," ",var.Bayesian1[67],");
               t8~ N(",est.Bayesian1[68]," ",var.Bayesian1[68],");
               t9~ N(",est.Bayesian1[69]," ",var.Bayesian1[69],");")
      
      
      
      
      body.model.Bayesian2 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian2,
        MODELPRIORS = MODELPRIORS21,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian2Trait_ADNILan_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                   modelout = paste0("Sim2_Bayesian2_ADNILan_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      est.Bayesian2=readModels(paste0("sim2_Bayesian2_ADNILan_imp_",rep,".out"))$parameters$unstandardized[,3]
      model.Bayesian3 <- paste0("th1_1  by  y4* y5 y6(l_1-l_3) 
                                y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3],";
                                th1_2  by  y10* y11 y12(l_1-l_3) 
                                y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y10@1 (n4);
	                     th2_5  by  y5@1 y11@1 (n5);
	                     th2_6  by  y6@1 y12@1 (n6);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       
                       [y1$1@",est.Bayesian2[61]," ","y7$1@",est.Bayesian2[61],"];
                       [y1$2@",est.Bayesian2[62]," ", "y7$2@",est.Bayesian2[62],"];
                       [y1$3@",est.Bayesian2[63]," ", "y7$3@",est.Bayesian2[63],"];
                       [y2$1@",est.Bayesian2[64]," ", "y8$1@",est.Bayesian2[64],"];
                       [y2$2@",est.Bayesian2[65]," ", "y8$2@",est.Bayesian2[65],"];
                       [y2$3@",est.Bayesian2[66]," ", "y8$3@",est.Bayesian2[66],"];
                       [y3$1@",est.Bayesian2[67]," ", "y9$1@",est.Bayesian2[67],"];
                       [y3$2@",est.Bayesian2[68]," ", "y9$2@",est.Bayesian2[68],"];
                       [y3$3@",est.Bayesian2[69]," ", "y9$3@",est.Bayesian2[69],"];
                       [y4$1 y10$1] (t10);
                       [y4$2 y10$2] (t11);
                       [y4$3 y10$3] (t12);
	                     [y5$1 y11$1] (t13);
	                     [y5$2 y11$2] (t14);
	                     [y5$3 y11$3] (t15);
	                     [y6$1 y12$1] (t16);
	                     [y6$2 y12$2] (t17);
	                     [y6$3 y12$3] (t18);
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
      
      
      body.model.Bayesian3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian3Trait_ADNILan_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                   modelout = paste0("Sim2_Bayesian3_ADNILan_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
    }
    
    
    
    # 3. Ignore repeated measure, fixed item parameters 
    setwd("F:/PsychMethod/Condition1Sim2IgnoreADNILan")
    setwd("~/")
    for (rep in 1:10){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      
      coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
      coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
      colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      #coh12.resp=rbind(coh1.resp,coh2.resp)
      model.Ignore1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     [th1_1@0];
                       th1_1@1;"
      
      body.model.Ignore1 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore1,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore1Trait_ADNILan_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                                   modelout = paste0("Sim2_Ignore1_ADNILan_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      est.Ignore1=readModels(paste0("Sim2_Ignore1_ADNILan_imp_",rep,".out"))$parameters$unstandardized[,3]
      #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
      
      model.Ignore2 <- paste0(" th1_1  by  y1@",est.Ignore1[1], " y2@",est.Ignore1[2], " y3@",est.Ignore1[3], " y4",
                              " y5"," y6"," ;",
                              "[th1_1*0];
                       th1_1*1;
	                     [y1$1@",est.Ignore1[8],"];
                       [y1$2@",est.Ignore1[9],"];
                       [y1$3@",est.Ignore1[10],"];
	                     [y2$1@",est.Ignore1[11],"];
	                     [y2$2@",est.Ignore1[12],"];
	                     [y2$3@",est.Ignore1[13],"];
	                     [y3$1@",est.Ignore1[14],"];
	                     [y3$2@",est.Ignore1[15],"];
	                     [y3$3@",est.Ignore1[16],"];")
      
      body.model.Ignore2 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore2,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore2Trait_ADNILan_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                                   modelout = paste0("Sim2_Ignore2_ADNILan_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      est.Ignore2=readModels(paste0("Sim2_Ignore2_ADNILan_imp_",rep,".out"))$parameters$unstandardized[,3]
      
      ####################
      #     Follow up
      ####################
      model.Ignore3 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3], " y10@",est.Ignore1[4],
                              " y11@",est.Ignore1[5]," y12@",est.Ignore1[6]," ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore1[8],"];
                       [y7$2@",est.Ignore1[9],"];
                       [y7$3@",est.Ignore1[10],"];
	                     [y8$1@",est.Ignore1[11],"];
	                     [y8$2@",est.Ignore1[12],"];
	                     [y8$3@",est.Ignore1[13],"];
	                     [y9$1@",est.Ignore1[14],"];
	                     [y9$2@",est.Ignore1[15],"];
	                     [y9$3@",est.Ignore1[16],"];
	                     [y10$1@",est.Ignore1[17],"];
	                     [y10$2@",est.Ignore1[18],"];
	                     [y10$3@",est.Ignore1[19],"];
	                     [y11$1@",est.Ignore1[20],"];
	                     [y11$2@",est.Ignore1[21],"];
	                     [y11$3@",est.Ignore1[22],"];
	                     [y12$1@",est.Ignore1[23],"];
	                     [y12$2@",est.Ignore1[24],"];
	                     [y12$3@",est.Ignore1[25],"];")
      
      body.model.Ignore3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore3Trait_ADNILan_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp.fl),
        rdata 	= coh1.resp.fl)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                                   modelout = paste0("Sim2_Ignore3_ADNILan_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
      model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore2[1], " y8@",est.Ignore2[2], " y9@",est.Ignore2[3], " y10@",est.Ignore2[4],
                              " y11@",est.Ignore2[5]," y12@",est.Ignore2[6]," ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore2[8],"];
                       [y7$2@",est.Ignore2[9],"];
                       [y7$3@",est.Ignore2[10],"];
	                     [y8$1@",est.Ignore2[11],"];
	                     [y8$2@",est.Ignore2[12],"];
	                     [y8$3@",est.Ignore2[13],"];
	                     [y9$1@",est.Ignore2[14],"];
	                     [y9$2@",est.Ignore2[15],"];
	                     [y9$3@",est.Ignore2[16],"];
	                     [y10$1@",est.Ignore2[17],"];
	                     [y10$2@",est.Ignore2[18],"];
	                     [y10$3@",est.Ignore2[19],"];
	                     [y11$1@",est.Ignore2[20],"];
	                     [y11$2@",est.Ignore2[21],"];
	                     [y11$3@",est.Ignore2[22],"];
	                     [y12$1@",est.Ignore2[23],"];
	                     [y12$2@",est.Ignore2[24],"];
	                     [y12$3@",est.Ignore2[25],"];")
      
      body.model.Ignore4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore4Trait_ADNILan_imp",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp.fl),
        rdata 	= coh2.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                   modelout = paste0("Sim2_Ignore4_ADNILan_imp_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
    } 
    
####################
#        2.1
####################  
    setwd("~/")
    setwd("/Users/zhux0445/OneDrive - UW/ADNI-Bayesian/Psy Methods")
    
    J=9
    1.1/1.7
    2.8/1.7
    set.seed(1)
    Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
    Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
    Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
    set.seed(100)
    Gamma1=runif(3,0.647,1.647) #0.9547661 0.9046725 1.1993224 
    gra=matrix(0,12,5)
    gra[1:6,1]= Amat1*1.7
    gra[7:12,2] = Amat2*1.7
    for (j in 1:3){
      gra[j,j+2]=gra[j+6,j+2]=Gamma1[j]*1.7
    }
    
    set.seed(1)
    grb1<-runif(J,-3,-1)
    grb2<-runif(J,-1,1)
    grb3<-runif(J,1,3)
    
    
    grb<-matrix(c(grb1,grb2,grb3),nrow=J)
    grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
    grorb
    round(grorb,3)
    grd=rbind(-grorb[1:6,]*1.7,-grorb[1:3,]*1.7,-grorb[7:9,]*1.7)
    
    N1=800
    N2=800
    N=N1+N2
    reps=50
    Theta1.all=matrix(0,reps*N1,3)
    for (rep in 1:reps){
      Theta1=rnorm(N1,0,1)
      Theta1n=cbind(1:N1,Theta1,Theta1)
      Theta1n[which(Theta1n[,2]<sort(Theta1)[200]),3]=(Theta1n[which(Theta1n[,2]<sort(Theta1)[200]),3]-0.2)
      Theta1n[which(Theta1n[,2]>sort(Theta1)[600]),3]=(Theta1n[which(Theta1n[,2]>sort(Theta1)[600]),3]+0.2)
      Theta1.all[((rep-1)*N1+1):(rep*N1),]=Theta1n
    }
    
    Theta1.all=matrix(0,reps*N1,3)
    for (rep in 1:reps){
      Theta1=rnorm(N1,0,1)
      disturbance1=runif(N1,-0.2,0.2)
      Theta1n=cbind(1:N1,Theta1,Theta1)
      Theta1n[1:200,3]=(Theta1n[1:200,3]-0.75+disturbance1[1:200])
      Theta1n[201:600,3]= Theta1n[201:600,3]+disturbance1[201:600]
      Theta1n[601:800,3]=(Theta1n[601:800,3]+0.75+disturbance1[601:800])
      Theta1.all[((rep-1)*N1+1):(rep*N1),]=Theta1n
    }
    cor(Theta1n[,2],Theta1n[,3])
    
    #Theta1=rnorm(N1,0,1)
    #Theta1n=cbind(1:N1,Theta1,Theta1)
    #Theta1n[which(Theta1n[,2]<sort(Theta1)[200]),3]=(Theta1n[which(Theta1n[,2]<sort(Theta1)[200]),3]-0.2)
    #Theta1n[which(Theta1n[,2]>sort(Theta1)[600]),3]=(Theta1n[which(Theta1n[,2]>sort(Theta1)[600]),3]+0.5)
    #Theta1.all[((rep-1)*N1+1):(rep*N1),]=Theta1n
    
    
    #for (rep in 1:reps){
    #  Theta1=rnorm(N1,0,1)
    #  Theta1n=cbind(1:N1,Theta1,Theta1)
    #  Theta1n[which(Theta1n[,2]<sort(Theta1)[160]),3]=(Theta1n[which(Theta1n[,2]<sort(Theta1)[160]),3]-0.5)
    #  Theta1n[which((Theta1n[,2]<sort(Theta1)[320])&(Theta1n[,2]>sort(Theta1)[160])),3]=(Theta1n[which((Theta1n[,2]<sort(Theta1)[320])&(Theta1n[,2]>sort(Theta1)[160])),3]-0.25)
    #  Theta1n[which((Theta1n[,2]<sort(Theta1)[640])&(Theta1n[,2]>sort(Theta1)[480])),3]=(Theta1n[which((Theta1n[,2]<sort(Theta1)[640])&(Theta1n[,2]>sort(Theta1)[480])),3]+0.6)
    #  Theta1n[which(Theta1n[,2]>sort(Theta1)[640]),3]=(Theta1n[which(Theta1n[,2]>sort(Theta1)[640]),3]+1.2)
    #  Theta1.all[((rep-1)*N1+1):(rep*N1),]=Theta1n
    #}
    #cor(Theta1n[,2],Theta1n[,3])
    #mean(Theta1n[,2])
    #mean(Theta1n[,3])
    #var(Theta1n[,2])
    #var(Theta1n[,3])
    
    Theta2.all=matrix(0,reps*N2,3)
    for (rep in 1:reps){
      Theta2=rnorm(N2,0.1,1)
      Theta2n=cbind(801:1600,Theta2,Theta2)
      Theta2n[which(Theta2n[,2]<sort(Theta2)[200]),3]=(Theta2n[which(Theta2n[,2]<sort(Theta2)[200]),3]-0.2)
      Theta2n[which(Theta2n[,2]>sort(Theta2)[600]),3]=(Theta2n[which(Theta2n[,2]>sort(Theta2)[600]),3]+0.2)
      Theta2.all[((rep-1)*N2+1):(rep*N2),]=Theta2n
    }
    
    Theta2.all=matrix(0,reps*N2,3)
    for (rep in 1:reps){
      Theta2=rnorm(N2,0.1,1)
      disturbance2=runif(N1,-0.2,0.2)
      Theta2n=cbind(801:1600,Theta2,Theta2)
      Theta2n[1:200,3]=(Theta2n[1:200,3]-0.75+disturbance2[1:200])
      Theta2n[201:600,3]= Theta2n[201:600,3]+disturbance2[201:600]
      Theta2n[601:800,3]=(Theta2n[601:800,3]+0.75+disturbance2[601:800])
      Theta2.all[((rep-1)*N1+1):(rep*N1),]=Theta2n
    }
    
    
    
    Theta.nuisance=matrix(rnorm(N*reps*3,0,1),N*reps,3)
    
    write.csv(Theta1.all,file='Condition2_trait_cohort1_shorttest_hard_clus_lowcor.csv')
    write.csv(Theta2.all,file='Condition2_trait_cohort2_shorttest_hard_clus_lowcor.csv')
    write.csv(Theta.nuisance,file='Condition2_trait_nuisance_cohort12_shorttest_hard_clus_lowcor.csv')
    
    
    
    
    #items <- c(rep('2PL',3),'graded','2PL')
    #dataset=simdata(Amat1,Dmat1,itemtype = items,Theta=matrix(Theta))
    
    #cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
    
    #cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
    
    
    cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1.all[,2:3],Theta.nuisance[1:40000,]))
    
    cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2.all[,2:3],Theta.nuisance[40001:80000,]))
    
    #colnames(resp.all)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
    #                     "y13","y14","y15","y16","y17","y18","y19","y20","y21","y22","y23","y24")
    #resp.all=as.data.frame(resp.all)
    #resp.all <- resp.all[order(resp.all$rid),]
    
    write.csv(cohort1.resp,file='sim2_cohort1_resp_shorttest_hard_clus_lowcor.csv')
    write.csv(cohort2.resp,file='sim2_cohort2_resp_shorttest_hard_clus_lowcor.csv')
    
    
    cohort1.resp=read.csv(file='sim2_cohort1_resp_shorttest_hard_clus_lowcor.csv')[,-1]
    cohort2.resp=read.csv(file='sim2_cohort2_resp_shorttest_hard_clus_lowcor.csv')[,-1]
    reps=50
    setwd("F:/PsychMethod/Simulation3_clus")
    
    # 1. Concurrent calibration (provided that the model converges)
    N1=N2=800
    for (rep in 1:50){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      coh12.resp=rbind(coh1.resp,coh2.resp)
      coh12.resp=cbind(coh12.resp,c(rep(1,N1),rep(2,N2)))
      colnames(coh12.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","Phase")
      
      model.concurrent <- " %OVERALL%
                       th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	         
                       
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     
	                   
	                     th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;
  
                        %c#1%
  	                     [th1_1@0];
                         th1_1 @1;
                         [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;
                       
  	                     %c#2%
                         [th1_1*0];
                         th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;"
      
      body.model.concurrent <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid; CLASSES=c(2); KNOWNCLASS=c(Phase=1-2);",
        ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.concurrent,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_ConcurrentTrait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh12.resp),
        rdata 	= coh12.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                                   modelout = paste0("Sim2_Concurrent_shorttest_hard_clus_lowcor",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
    }
    
    # 2. Bayesian estimation (using informative priors)
    for (rep in 1:50){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      
      model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
      
      
      body.model.Bayesian1 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Bayesian1,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian1Trait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                   modelout = paste0("Sim2_shorttest_Bayesian1_hard_clus_lowcor_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      est.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_clus_lowcor_",rep,".out"))$parameters$unstandardized[,3]
      var.Bayesian1=readModels(paste0("sim2_shorttest_bayesian1_hard_clus_lowcor_",rep,".out"))$parameters$unstandardized[,4]^2
      #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
      
      model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1] (t10);
                       [y4$2] (t11);
                       [y4$3] (t12);
	                     [y5$1] (t13);
                       [y5$2] (t14);
                       [y5$3] (t15);
                       [y6$1] (t16);
                       [y6$2] (t17);
                       [y6$3] (t18);
	                     [y10$1] (t19);
                       [y10$2] (t20);
                       [y10$3] (t21);
                       [y11$1] (t22);
                       [y11$2] (t23);
                       [y11$3] (t24);
                       [y12$1] (t25);
                       [y12$2] (t26);
                       [y12$3] (t27);
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0;"
      
      
      
      
      MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               l_4~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_5~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_6~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_7~ N(",est.Bayesian1[10]," ",var.Bayesian1[10],");
               l_8~ N(",est.Bayesian1[11]," ",var.Bayesian1[11],");
               l_9~ N(",est.Bayesian1[12]," ",var.Bayesian1[12],");
               
               t1~ N(",est.Bayesian1[34]," ",var.Bayesian1[34],");
               t2~ N(",est.Bayesian1[35]," ",var.Bayesian1[35],");
               t3~ N(",est.Bayesian1[36]," ",var.Bayesian1[36],");
               t4~ N(",est.Bayesian1[37]," ",var.Bayesian1[37],");
               t5~ N(",est.Bayesian1[38]," ",var.Bayesian1[38],");
               t6~ N(",est.Bayesian1[39]," ",var.Bayesian1[39],");
               t7~ N(",est.Bayesian1[40]," ",var.Bayesian1[40],");
               t8~ N(",est.Bayesian1[41]," ",var.Bayesian1[41],");
               t9~ N(",est.Bayesian1[42]," ",var.Bayesian1[42],");
               t10~ N(",est.Bayesian1[43]," ",var.Bayesian1[43],");
               t11~ N(",est.Bayesian1[44]," ",var.Bayesian1[44],");
               t12~ N(",est.Bayesian1[45]," ",var.Bayesian1[45],");
               t13~ N(",est.Bayesian1[46]," ",var.Bayesian1[46],");
               t14~ N(",est.Bayesian1[47]," ",var.Bayesian1[47],");
               t15~ N(",est.Bayesian1[48]," ",var.Bayesian1[48],");
               t16~ N(",est.Bayesian1[49]," ",var.Bayesian1[49],");
               t17~ N(",est.Bayesian1[50]," ",var.Bayesian1[50],");
               t18~ N(",est.Bayesian1[51]," ",var.Bayesian1[51],");
               t19~ N(",est.Bayesian1[61]," ",var.Bayesian1[61],");
               t20~ N(",est.Bayesian1[62]," ",var.Bayesian1[62],");
               t21~ N(",est.Bayesian1[63]," ",var.Bayesian1[63],");
               t22~ N(",est.Bayesian1[64]," ",var.Bayesian1[64],");
               t23~ N(",est.Bayesian1[65]," ",var.Bayesian1[65],");
               t24~ N(",est.Bayesian1[66]," ",var.Bayesian1[66],");
               t25~ N(",est.Bayesian1[67]," ",var.Bayesian1[67],");
               t26~ N(",est.Bayesian1[68]," ",var.Bayesian1[68],");
               t27~ N(",est.Bayesian1[69]," ",var.Bayesian1[69],");")
      
      
      
      
      body.model.Bayesian2 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Bayesian2,
        MODELPRIORS = MODELPRIORS21,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian2Trait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                   modelout = paste0("Sim2_shorttest_Bayesian2_hard_clus_lowcor_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      est.Bayesian2=readModels(paste0("sim2_shorttest_Bayesian2_hard_clus_lowcor_",rep,".out"))$parameters$unstandardized[,3]
      model.Bayesian3 <- paste0("th1_1  by  y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3]," ","y4@",est.Bayesian2[4]," 
                            y5@",est.Bayesian2[5]," ","y6@",est.Bayesian2[6],";
                            th1_2  by  y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3]," ","y10@",est.Bayesian2[10]," 
                            y11@",est.Bayesian2[11]," ","y12@",est.Bayesian2[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian2[34]," ","y7$1@",est.Bayesian2[34],"];
                       [y1$2@",est.Bayesian2[35]," ", "y7$2@",est.Bayesian2[35],"];
                       [y1$3@",est.Bayesian2[36]," ", "y7$3@",est.Bayesian2[36],"];
                       [y2$1@",est.Bayesian2[37]," ", "y8$1@",est.Bayesian2[37],"];
                       [y2$2@",est.Bayesian2[38]," ", "y8$2@",est.Bayesian2[38],"];
                       [y2$3@",est.Bayesian2[39]," ", "y8$3@",est.Bayesian2[39],"];
                       [y3$1@",est.Bayesian2[40]," ", "y9$1@",est.Bayesian2[40],"];
                       [y3$2@",est.Bayesian2[41]," ", "y9$2@",est.Bayesian2[41],"];
                       [y3$3@",est.Bayesian2[42]," ", "y9$3@",est.Bayesian2[42],"];
                       [y4$1@",est.Bayesian2[43],"];
                       [y4$2@",est.Bayesian2[44],"];
                       [y4$3@",est.Bayesian2[45],"];
                       [y5$1@",est.Bayesian2[46],"];
                       [y5$2@",est.Bayesian2[47],"];
                       [y5$3@",est.Bayesian2[48],"];
                       [y6$1@",est.Bayesian2[49],"];
                       [y6$2@",est.Bayesian2[50],"];
                       [y6$3@",est.Bayesian2[51],"];
                       [y10$1@",est.Bayesian2[61],"];
                       [y10$2@",est.Bayesian2[62],"];
                       [y10$3@",est.Bayesian2[63],"];
                       [y11$1@",est.Bayesian2[64],"];
                       [y11$2@",est.Bayesian2[65],"];
                       [y11$3@",est.Bayesian2[66],"];
                       [y12$1@",est.Bayesian2[67],"];
                       [y12$2@",est.Bayesian2[68],"];
                       [y12$3@",est.Bayesian2[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3")
      
      
      body.model.Bayesian3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Bayesian3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2_Bayesian3Trait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                   modelout = paste0("Sim2_shorttest_Bayesian3_hard_clus_lowcor_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      
      
    }
    # 3. Ignore repeated measure, fixed item parameters 
    
    for (rep in 1:50){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      
      coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
      coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
      colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      #coh12.resp=rbind(coh1.resp,coh2.resp)
      model.Ignore1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     [th1_1@0];
                       th1_1@1;"
      
      body.model.Ignore1 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore1,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore1Trait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                                   modelout = paste0("Sim2_shorttest_Ignore1_hard_clus_lowcor_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      est.Ignore1=readModels(paste0("Sim2_shorttest_Ignore1_hard_clus_lowcor_",rep,".out"))$parameters$unstandardized[,3]
      #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
      
      model.Ignore2 <- paste0(" th1_1  by  y1@",est.Ignore1[1], " y2@",est.Ignore1[2], " y3@",est.Ignore1[3], " y4@",est.Ignore1[4],
                              " y5@",est.Ignore1[5]," y6@",est.Ignore1[6]," ;",
                              "[th1_1*0];
                       th1_1*1;
	                     [y1$1@",est.Ignore1[8],"];
                       [y1$2@",est.Ignore1[9],"];
                       [y1$3@",est.Ignore1[10],"];
	                     [y2$1@",est.Ignore1[11],"];
	                     [y2$2@",est.Ignore1[12],"];
	                     [y2$3@",est.Ignore1[13],"];
	                     [y3$1@",est.Ignore1[14],"];
	                     [y3$2@",est.Ignore1[15],"];
	                     [y3$3@",est.Ignore1[16],"];
	                     [y4$1@",est.Ignore1[17],"];
	                     [y4$2@",est.Ignore1[18],"];
	                     [y4$3@",est.Ignore1[19],"];
	                     [y5$1@",est.Ignore1[20],"];
	                     [y5$2@",est.Ignore1[21],"];
	                     [y5$3@",est.Ignore1[22],"];
	                     [y6$1@",est.Ignore1[23],"];
	                     [y6$2@",est.Ignore1[24],"];
	                     [y6$3@",est.Ignore1[25],"];")
      
      body.model.Ignore2 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore2,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore2Trait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                                   modelout = paste0("Sim2_shorttest_Ignore2_hard_clus_lowcor_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      ####################
      #     Follow up
      ####################
      model.Ignore3 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3],
                              "
                          y10 y11 y12;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore1[8],"];
                       [y7$2@",est.Ignore1[9],"];
                       [y7$3@",est.Ignore1[10],"];
	                     [y8$1@",est.Ignore1[11],"];
	                     [y8$2@",est.Ignore1[12],"];
	                     [y8$3@",est.Ignore1[13],"];
	                     [y9$1@",est.Ignore1[14],"];
	                     [y9$2@",est.Ignore1[15],"];
	                     [y9$3@",est.Ignore1[16],"];")
      
      body.model.Ignore3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore3Trait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp.fl),
        rdata 	= coh1.resp.fl)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                                   modelout = paste0("Sim2_shorttest_Ignore3_hard_clus_lowcor_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
      est.Ignore2=readModels(paste0("Sim2_shorttest_Ignore3_hard_clus_lowcor_",rep,".out"))$parameters$unstandardized[,3]
      model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore2[1], " y8@",est.Ignore2[2], " y9@",est.Ignore1[3],"
                          y10@", est.Ignore2[4], " y11@",est.Ignore2[5], "  y12@", est.Ignore2[6], " ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore2[8],"];
                       [y7$2@",est.Ignore2[9],"];
                       [y7$3@",est.Ignore2[10],"];
	                     [y8$1@",est.Ignore2[11],"];
	                     [y8$2@",est.Ignore2[12],"];
	                     [y8$3@",est.Ignore2[13],"];
	                     [y9$1@",est.Ignore2[14],"];
	                     [y9$2@",est.Ignore2[15],"];
	                     [y9$3@",est.Ignore2[16],"];
                       [y10$1@",est.Ignore2[17],"];
                       [y10$2@",est.Ignore2[18],"];
                       [y10$3@",est.Ignore2[19],"];
	                     [y11$1@",est.Ignore2[20],"];
	                     [y11$2@",est.Ignore2[21],"];
	                     [y11$3@",est.Ignore2[22],"];
	                     [y12$1@",est.Ignore2[23],"];
	                     [y12$2@",est.Ignore2[24],"];
	                     [y12$3@",est.Ignore2[25],"];")
      body.model.Ignore4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim2Ignore4Trait_shorttest_hard_clus_lowcor",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp.fl),
        rdata 	= coh2.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                   modelout = paste0("Sim2_shorttest_Ignore4_hard_clus_lowcor_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
    }
    
  