######################################
# 1/19/2023 
# Psy method additional simulation
######################################

#############################
#
#      3-cohort design part 1 
#
#############################

#####################################################
# 
#      Sim 3.1   Item design 1 + person design 1
#
#####################################################


setwd("F:/PsychMethod/sim31")

library(mvtnorm)
library(mirt)
library(MplusAutomation)

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
N3=800
N=N1+N2+N3
reps=50
set.seed(1)
Theta1=rmvnorm(N1*reps,c(0,0.2),matrix(c(1,0.4,0.4,1),2,2))
Theta2=rmvnorm(N2*reps,c(0.1,0.3),matrix(c(1,0.4,0.4,1),2,2))
Theta3=rmvnorm(N3*reps,c(0.2,0.4),matrix(c(1,0.4,0.4,1),2,2))
Theta.nuisance=matrix(rnorm(N*reps*J,0,1),N*reps,3)


hist(Theta1[,2]-Theta1[,1])
hist(Theta2[,2]-Theta2[,1])
hist(Theta3[,2]-Theta3[,1])

write.csv(Theta1,file='Condition31_trait_cohort1.csv')
write.csv(Theta2,file='Condition31_trait_cohort2.csv')
write.csv(Theta3,file='Condition31_trait_cohort3.csv')
write.csv(Theta.nuisance,file='Condition31_trait_nuisance_cohort123.csv')

cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
cohort3.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta3,Theta.nuisance[80001:120000,]))


write.csv(cohort1.resp,file='sim31_cohort1_resp.csv')
write.csv(cohort2.resp,file='sim31_cohort2_resp.csv')
write.csv(cohort3.resp,file='sim31_cohort3_resp.csv')


cohort1.resp=read.csv(file='sim31_cohort1_resp.csv')[,-1]
cohort2.resp=read.csv(file='sim31_cohort2_resp.csv')[,-1]
cohort3.resp=read.csv(file='sim31_cohort3_resp.csv')[,-1]


# 1. Concurrent calibration (provided that the model converges)
N1=N2=N3=800
for (rep in 1:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  coh123.resp=rbind(coh1.resp,coh2.resp,coh3.resp)
  coh123.resp=cbind(coh123.resp,c(rep(1,N1),rep(2,N2),rep(3,N3)))
  colnames(coh123.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","Phase")
  
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
                       th1_1 WITH th1_2;
                       
                        %c#3%
                         [th1_1*0];
                         th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;"
  
  body.model.concurrent <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid; CLASSES=c(3); KNOWNCLASS=c(Phase=1-3);",
    ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.concurrent,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim31_ConcurrentTrait_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh123.resp),
    rdata 	= coh123.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                               modelout = paste0("Sim31_Concurrent_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
}


# 2. Bayesian estimation (using informative priors)
for (rep in 1:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
  colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
 
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
    SAVEDATA 	= paste0("FILE is Sim31_BayesianTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                               modelout = paste0("Sim31_Bayesian1_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Bayesian1=readModels(paste0("Sim31_Bayesian1_",rep,".out"))$parameters$unstandardized[,3]
  var.Bayesian1=readModels(paste0("Sim31_Bayesian1_",rep,".out"))$parameters$unstandardized[,4]^2
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
    SAVEDATA 	= paste0("FILE is Sim31_BayesianTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                               modelout = paste0("Sim31_Bayesian2_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Bayesian2=readModels(paste0("Sim31_Bayesian2_",rep,".out"))$parameters$unstandardized[,3]
  var.Bayesian2=readModels(paste0("Sim31_Bayesian2_",rep,".out"))$parameters$unstandardized[,4]^2
  #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
  
  model.Bayesian3 <-" th1_1  by  y1-y6* (l_1-l_6);
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
  
  
  
  
  MODELPRIORS31 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
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
  
  
  
  
  body.model.Bayesian3 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian3,
    MODELPRIORS = MODELPRIORS31,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim31_BayesianTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh3.resp),
    rdata 	= coh3.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                               modelout = paste0("Sim31_Bayesian3_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')
  
  # re-estimate cohort 1
  
  est.Bayesian3=readModels(paste0("Sim31_Bayesian3_",rep,".out"))$parameters$unstandardized[,3]
  model.Bayesian4 <- paste0("th1_1  by  y1@",est.Bayesian3[1]," ","y2@",est.Bayesian3[2]," ","y3@",est.Bayesian3[3]," ","y4@",est.Bayesian3[4]," 
                            y5@",est.Bayesian3[5]," ","y6@",est.Bayesian3[6],";
                            th1_2  by  y7@",est.Bayesian3[1]," ","y8@",est.Bayesian3[2]," ","y9@",est.Bayesian3[3]," ","y10@",est.Bayesian3[10]," 
                            y11@",est.Bayesian3[11]," ","y12@",est.Bayesian3[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian3[34]," ","y7$1@",est.Bayesian3[34],"];
                       [y1$2@",est.Bayesian3[35]," ", "y7$2@",est.Bayesian3[35],"];
                       [y1$3@",est.Bayesian3[36]," ", "y7$3@",est.Bayesian3[36],"];
                       [y2$1@",est.Bayesian3[37]," ", "y8$1@",est.Bayesian3[37],"];
                       [y2$2@",est.Bayesian3[38]," ", "y8$2@",est.Bayesian3[38],"];
                       [y2$3@",est.Bayesian3[39]," ", "y8$3@",est.Bayesian3[39],"];
                       [y3$1@",est.Bayesian3[40]," ", "y9$1@",est.Bayesian3[40],"];
                       [y3$2@",est.Bayesian3[41]," ", "y9$2@",est.Bayesian3[41],"];
                       [y3$3@",est.Bayesian3[42]," ", "y9$3@",est.Bayesian3[42],"];
                       [y4$1@",est.Bayesian3[43],"];
                       [y4$2@",est.Bayesian3[44],"];
                       [y4$3@",est.Bayesian3[45],"];
                       [y5$1@",est.Bayesian3[46],"];
                       [y5$2@",est.Bayesian3[47],"];
                       [y5$3@",est.Bayesian3[48],"];
                       [y6$1@",est.Bayesian3[49],"];
                       [y6$2@",est.Bayesian3[50],"];
                       [y6$3@",est.Bayesian3[51],"];
                       [y10$1@",est.Bayesian3[61],"];
                       [y10$2@",est.Bayesian3[62],"];
                       [y10$3@",est.Bayesian3[63],"];
                       [y11$1@",est.Bayesian3[64],"];
                       [y11$2@",est.Bayesian3[65],"];
                       [y11$3@",est.Bayesian3[66],"];
                       [y12$1@",est.Bayesian3[67],"];
                       [y12$2@",est.Bayesian3[68],"];
                       [y12$3@",est.Bayesian3[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0")
  
  
  body.model.Bayesian4 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian4,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim31_BayesianTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian4, 
                                               modelout = paste0("Sim31_Bayesian4_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  # re-estimate cohort 2 
  
  model.Bayesian5 <- paste0("th1_1  by  y1@",est.Bayesian3[1]," ","y2@",est.Bayesian3[2]," ","y3@",est.Bayesian3[3]," ","y4@",est.Bayesian3[4]," 
                            y5@",est.Bayesian3[5]," ","y6@",est.Bayesian3[6],";
                            th1_2  by  y7@",est.Bayesian3[1]," ","y8@",est.Bayesian3[2]," ","y9@",est.Bayesian3[3]," ","y10@",est.Bayesian3[10]," 
                            y11@",est.Bayesian3[11]," ","y12@",est.Bayesian3[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian3[34]," ","y7$1@",est.Bayesian3[34],"];
                       [y1$2@",est.Bayesian3[35]," ", "y7$2@",est.Bayesian3[35],"];
                       [y1$3@",est.Bayesian3[36]," ", "y7$3@",est.Bayesian3[36],"];
                       [y2$1@",est.Bayesian3[37]," ", "y8$1@",est.Bayesian3[37],"];
                       [y2$2@",est.Bayesian3[38]," ", "y8$2@",est.Bayesian3[38],"];
                       [y2$3@",est.Bayesian3[39]," ", "y8$3@",est.Bayesian3[39],"];
                       [y3$1@",est.Bayesian3[40]," ", "y9$1@",est.Bayesian3[40],"];
                       [y3$2@",est.Bayesian3[41]," ", "y9$2@",est.Bayesian3[41],"];
                       [y3$3@",est.Bayesian3[42]," ", "y9$3@",est.Bayesian3[42],"];
                       [y4$1@",est.Bayesian3[43],"];
                       [y4$2@",est.Bayesian3[44],"];
                       [y4$3@",est.Bayesian3[45],"];
                       [y5$1@",est.Bayesian3[46],"];
                       [y5$2@",est.Bayesian3[47],"];
                       [y5$3@",est.Bayesian3[48],"];
                       [y6$1@",est.Bayesian3[49],"];
                       [y6$2@",est.Bayesian3[50],"];
                       [y6$3@",est.Bayesian3[51],"];
                       [y10$1@",est.Bayesian3[61],"];
                       [y10$2@",est.Bayesian3[62],"];
                       [y10$3@",est.Bayesian3[63],"];
                       [y11$1@",est.Bayesian3[64],"];
                       [y11$2@",est.Bayesian3[65],"];
                       [y11$3@",est.Bayesian3[66],"];
                       [y12$1@",est.Bayesian3[67],"];
                       [y12$2@",est.Bayesian3[68],"];
                       [y12$3@",est.Bayesian3[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0")
  
  body.model.Bayesian5 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian5,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim31_BayesianTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian5, 
                                               modelout = paste0("Sim31_Bayesian5_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
}



# 3. Ignore repeated measure, fixed item parameters 

for (rep in 1:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
  coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),1:6])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  
  coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
  coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
  coh3.resp.fl=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),7:12])
  colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
  colnames(coh3.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
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
    SAVEDATA 	= paste0("FILE is Sim31_IgnoreTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                               modelout = paste0("Sim31_Ignore1_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Ignore1=readModels(paste0("Sim31_Ignore1_",rep,".out"))$parameters$unstandardized[,3]
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
    SAVEDATA 	= paste0("FILE is Sim31_IgnoreTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                               modelout = paste0("Sim31_Ignore2_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  #cohort 3 baseline
  model.Ignore3 <- paste0(" th1_1  by  y1@",est.Ignore1[1], " y2@",est.Ignore1[2], " y3@",est.Ignore1[3], " y4@",est.Ignore1[4],
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
  
  body.model.Ignore3 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore3,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim31_IgnoreTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh3.resp),
    rdata 	= coh3.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                               modelout = paste0("Sim31_Ignore3_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')
  
  ####################
  #     Cohort 1 Follow up
  ####################
  model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3],
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
  
  body.model.Ignore4 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore4,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim31_IgnoreTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp.fl),
    rdata 	= coh1.resp.fl)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                               modelout = paste0("Sim31_Ignore4_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always') 

  # cohort 2 follow-up
    
    est.Ignore4=readModels(paste0("Sim31_Ignore4_",rep,".out"))$parameters$unstandardized[,3]
    
    model.Ignore5 <- paste0(" th1_1  by  y7@",est.Ignore4[1], " y8@",est.Ignore4[2], " y9@",est.Ignore4[3],"
                          y10@", est.Ignore4[4], " y11@",est.Ignore4[5], "  y12@", est.Ignore4[6], " ;",
                            "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore4[8],"];
                       [y7$2@",est.Ignore4[9],"];
                       [y7$3@",est.Ignore4[10],"];
	                     [y8$1@",est.Ignore4[11],"];
	                     [y8$2@",est.Ignore4[12],"];
	                     [y8$3@",est.Ignore4[13],"];
	                     [y9$1@",est.Ignore4[14],"];
	                     [y9$2@",est.Ignore4[15],"];
	                     [y9$3@",est.Ignore4[16],"];
                       [y10$1@",est.Ignore4[17],"];
                       [y10$2@",est.Ignore4[18],"];
                       [y10$3@",est.Ignore4[19],"];
	                     [y11$1@",est.Ignore4[20],"];
	                     [y11$2@",est.Ignore4[21],"];
	                     [y11$3@",est.Ignore4[22],"];
	                     [y12$1@",est.Ignore4[23],"];
	                     [y12$2@",est.Ignore4[24],"];
	                     [y12$3@",est.Ignore4[25],"];")
    body.model.Ignore5 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore5,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim31_IgnoreTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp.fl),
      rdata 	= coh2.resp.fl)
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore5, 
                                                 modelout = paste0("Sim31_Ignore5_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')  
    # cohort 3 follow-up
    
    
    model.Ignore6 <- paste0(" th1_1  by  y7@",est.Ignore4[1], " y8@",est.Ignore4[2], " y9@",est.Ignore4[3],"
                          y10@", est.Ignore4[4], " y11@",est.Ignore4[5], "  y12@", est.Ignore4[6], " ;",
                            "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore4[8],"];
                       [y7$2@",est.Ignore4[9],"];
                       [y7$3@",est.Ignore4[10],"];
	                     [y8$1@",est.Ignore4[11],"];
	                     [y8$2@",est.Ignore4[12],"];
	                     [y8$3@",est.Ignore4[13],"];
	                     [y9$1@",est.Ignore4[14],"];
	                     [y9$2@",est.Ignore4[15],"];
	                     [y9$3@",est.Ignore4[16],"];
                       [y10$1@",est.Ignore4[17],"];
                       [y10$2@",est.Ignore4[18],"];
                       [y10$3@",est.Ignore4[19],"];
	                     [y11$1@",est.Ignore4[20],"];
	                     [y11$2@",est.Ignore4[21],"];
	                     [y11$3@",est.Ignore4[22],"];
	                     [y12$1@",est.Ignore4[23],"];
	                     [y12$2@",est.Ignore4[24],"];
	                     [y12$3@",est.Ignore4[25],"];")
    body.model.Ignore6 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore6,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim31_IgnoreTrait6_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh3.resp.fl),
      rdata 	= coh3.resp.fl)
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore6, 
                                                 modelout = paste0("Sim31_Ignore6_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')  
  }
  


  #########################################################
  #
  #     sim 3.2       Item design 1 + person design 2
  #
  #########################################################


  setwd("F:/PsychMethod/sim32")
  N1=800
  N2=800
  N3=800
  N=N1+N2+N3
  reps=50
  set.seed(1)
  Theta1=rmvnorm(N1*reps,c(0,0.05),matrix(c(1,1.04,1.04,1.37),2,2))
  Theta2=rmvnorm(N2*reps,c(0.5,0.55),matrix(c(1.12,1.15,1.15,1.3),2,2))
  Theta3=rmvnorm(N3*reps,c(0.25,0.3),matrix(c(1,1.1,1.1,1.3),2,2))
  Theta.nuisance=matrix(rnorm(N*reps*3,0,1),N*reps,3)
  
  
  
  mean(Theta1[,2]-Theta1[,1])
  mean(Theta2[,2]-Theta2[,1])
  mean(Theta3[,2]-Theta3[,1])
  
  sd(Theta1[,2]-Theta1[,1])
  sd(Theta2[,2]-Theta2[,1])
  sd(Theta3[,2]-Theta3[,1])
  
  write.csv(Theta1,file='Condition32_trait_cohort1.csv')
  write.csv(Theta2,file='Condition32_trait_cohort2.csv')
  write.csv(Theta3,file='Condition32_trait_cohort3.csv')
  write.csv(Theta.nuisance,file='Condition32_trait_nuisance_cohort123.csv')
  
  
  cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
  cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
  cohort3.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta3,Theta.nuisance[80001:120000,]))
  
  #colnames(resp.all)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
  #                     "y13","y14","y15","y16","y17","y18","y19","y20","y21","y22","y23","y24")
  #resp.all=as.data.frame(resp.all)
  #resp.all <- resp.all[order(resp.all$rid),]
  
  write.csv(cohort1.resp,file='sim32_cohort1_resp.csv')
  write.csv(cohort2.resp,file='sim32_cohort2_resp.csv')
  write.csv(cohort3.resp,file='sim32_cohort3_resp.csv')
  
  
  cohort1.resp=read.csv(file='sim32_cohort1_resp.csv')[,-1]
  cohort2.resp=read.csv(file='sim32_cohort2_resp.csv')[,-1]
  cohort3.resp=read.csv(file='sim32_cohort3_resp.csv')[,-1]
  
  #cohort1.resp=read.csv(file='sim2_cohort1_resp.csv')
  #cohort2.resp=read.csv(file='sim2_cohort2_resp.csv')
  
  
  # 1. Concurrent calibration (provided that the model converges)
  N1=N2=N3=800
  for (rep in 1:reps){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
    coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    coh123.resp=rbind(coh1.resp,coh2.resp,coh3.resp)
    coh123.resp=cbind(coh123.resp,c(rep(1,N1),rep(2,N2),rep(3,N3)))
    colnames(coh123.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","Phase")
    
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
                       th1_1 WITH th1_2;
                       
                        %c#3%
                         [th1_1*0];
                         th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       th1_1 WITH th1_2;"
    
    body.model.concurrent <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid; CLASSES=c(3); KNOWNCLASS=c(Phase=1-3);",
      ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.concurrent,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_ConcurrentTrait_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh123.resp),
      rdata 	= coh123.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                                 modelout = paste0("Sim32_Concurrent_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
  }
  
  
  # 2. Bayesian estimation (using informative priors)
  for (rep in 1:reps){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
    coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
    
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
      SAVEDATA 	= paste0("FILE is Sim32_BayesianTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                 modelout = paste0("Sim32_Bayesian1_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
    est.Bayesian1=readModels(paste0("Sim32_Bayesian1_",rep,".out"))$parameters$unstandardized[,3]
    var.Bayesian1=readModels(paste0("Sim32_Bayesian1_",rep,".out"))$parameters$unstandardized[,4]^2
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
      SAVEDATA 	= paste0("FILE is Sim32_BayesianTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp),
      rdata 	= coh2.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                 modelout = paste0("Sim32_Bayesian2_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
    est.Bayesian2=readModels(paste0("Sim32_Bayesian2_",rep,".out"))$parameters$unstandardized[,3]
    var.Bayesian2=readModels(paste0("Sim32_Bayesian2_",rep,".out"))$parameters$unstandardized[,4]^2
    #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
    
    model.Bayesian3 <-" th1_1  by  y1-y6* (l_1-l_6);
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
    
    
    
    
    MODELPRIORS32 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
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
    
    
    
    
    body.model.Bayesian3 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian3,
      MODELPRIORS = MODELPRIORS32,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_BayesianTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh3.resp),
      rdata 	= coh3.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                 modelout = paste0("Sim32_Bayesian3_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')
    
    # re-estimate cohort 1
    
    est.Bayesian3=readModels(paste0("Sim32_Bayesian3_",rep,".out"))$parameters$unstandardized[,3]
    model.Bayesian4 <- paste0("th1_1  by  y1@",est.Bayesian3[1]," ","y2@",est.Bayesian3[2]," ","y3@",est.Bayesian3[3]," ","y4@",est.Bayesian3[4]," 
                            y5@",est.Bayesian3[5]," ","y6@",est.Bayesian3[6],";
                            th1_2  by  y7@",est.Bayesian3[1]," ","y8@",est.Bayesian3[2]," ","y9@",est.Bayesian3[3]," ","y10@",est.Bayesian3[10]," 
                            y11@",est.Bayesian3[11]," ","y12@",est.Bayesian3[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian3[34]," ","y7$1@",est.Bayesian3[34],"];
                       [y1$2@",est.Bayesian3[35]," ", "y7$2@",est.Bayesian3[35],"];
                       [y1$3@",est.Bayesian3[36]," ", "y7$3@",est.Bayesian3[36],"];
                       [y2$1@",est.Bayesian3[37]," ", "y8$1@",est.Bayesian3[37],"];
                       [y2$2@",est.Bayesian3[38]," ", "y8$2@",est.Bayesian3[38],"];
                       [y2$3@",est.Bayesian3[39]," ", "y8$3@",est.Bayesian3[39],"];
                       [y3$1@",est.Bayesian3[40]," ", "y9$1@",est.Bayesian3[40],"];
                       [y3$2@",est.Bayesian3[41]," ", "y9$2@",est.Bayesian3[41],"];
                       [y3$3@",est.Bayesian3[42]," ", "y9$3@",est.Bayesian3[42],"];
                       [y4$1@",est.Bayesian3[43],"];
                       [y4$2@",est.Bayesian3[44],"];
                       [y4$3@",est.Bayesian3[45],"];
                       [y5$1@",est.Bayesian3[46],"];
                       [y5$2@",est.Bayesian3[47],"];
                       [y5$3@",est.Bayesian3[48],"];
                       [y6$1@",est.Bayesian3[49],"];
                       [y6$2@",est.Bayesian3[50],"];
                       [y6$3@",est.Bayesian3[51],"];
                       [y10$1@",est.Bayesian3[61],"];
                       [y10$2@",est.Bayesian3[62],"];
                       [y10$3@",est.Bayesian3[63],"];
                       [y11$1@",est.Bayesian3[64],"];
                       [y11$2@",est.Bayesian3[65],"];
                       [y11$3@",est.Bayesian3[66],"];
                       [y12$1@",est.Bayesian3[67],"];
                       [y12$2@",est.Bayesian3[68],"];
                       [y12$3@",est.Bayesian3[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0")
    
    
    body.model.Bayesian4 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian4,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_BayesianTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian4, 
                                                 modelout = paste0("Sim32_Bayesian4_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    # re-estimate cohort 2 
    
    model.Bayesian5 <- paste0("th1_1  by  y1@",est.Bayesian3[1]," ","y2@",est.Bayesian3[2]," ","y3@",est.Bayesian3[3]," ","y4@",est.Bayesian3[4]," 
                            y5@",est.Bayesian3[5]," ","y6@",est.Bayesian3[6],";
                            th1_2  by  y7@",est.Bayesian3[1]," ","y8@",est.Bayesian3[2]," ","y9@",est.Bayesian3[3]," ","y10@",est.Bayesian3[10]," 
                            y11@",est.Bayesian3[11]," ","y12@",est.Bayesian3[12],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th2_1-th2_3@0];
                       th2_1-th2_3*1;
                       
                       [y1$1@",est.Bayesian3[34]," ","y7$1@",est.Bayesian3[34],"];
                       [y1$2@",est.Bayesian3[35]," ", "y7$2@",est.Bayesian3[35],"];
                       [y1$3@",est.Bayesian3[36]," ", "y7$3@",est.Bayesian3[36],"];
                       [y2$1@",est.Bayesian3[37]," ", "y8$1@",est.Bayesian3[37],"];
                       [y2$2@",est.Bayesian3[38]," ", "y8$2@",est.Bayesian3[38],"];
                       [y2$3@",est.Bayesian3[39]," ", "y8$3@",est.Bayesian3[39],"];
                       [y3$1@",est.Bayesian3[40]," ", "y9$1@",est.Bayesian3[40],"];
                       [y3$2@",est.Bayesian3[41]," ", "y9$2@",est.Bayesian3[41],"];
                       [y3$3@",est.Bayesian3[42]," ", "y9$3@",est.Bayesian3[42],"];
                       [y4$1@",est.Bayesian3[43],"];
                       [y4$2@",est.Bayesian3[44],"];
                       [y4$3@",est.Bayesian3[45],"];
                       [y5$1@",est.Bayesian3[46],"];
                       [y5$2@",est.Bayesian3[47],"];
                       [y5$3@",est.Bayesian3[48],"];
                       [y6$1@",est.Bayesian3[49],"];
                       [y6$2@",est.Bayesian3[50],"];
                       [y6$3@",est.Bayesian3[51],"];
                       [y10$1@",est.Bayesian3[61],"];
                       [y10$2@",est.Bayesian3[62],"];
                       [y10$3@",est.Bayesian3[63],"];
                       [y11$1@",est.Bayesian3[64],"];
                       [y11$2@",est.Bayesian3[65],"];
                       [y11$3@",est.Bayesian3[66],"];
                       [y12$1@",est.Bayesian3[67],"];
                       [y12$2@",est.Bayesian3[68],"];
                       [y12$3@",est.Bayesian3[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_3@0;
                       th1_2 WITH th2_1-th2_3@0;
                       th2_1 WITH th2_2-th2_3@0;
                       th2_2 WITH th2_3@0")
    
    body.model.Bayesian5 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Bayesian5,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_BayesianTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp),
      rdata 	= coh2.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian5, 
                                                 modelout = paste0("Sim32_Bayesian5_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
  }
  
  
  
  # 3. Ignore repeated measure, fixed item parameters 
  
  for (rep in 1:reps){
    coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
    coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
    coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),1:6])
    colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
    colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
    colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6")
    
    coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
    coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
    coh3.resp.fl=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),7:12])
    colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
    colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
    colnames(coh3.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
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
      SAVEDATA 	= paste0("FILE is Sim32_IgnoreTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp),
      rdata 	= coh1.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                                 modelout = paste0("Sim32_Ignore1_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    
    est.Ignore1=readModels(paste0("Sim32_Ignore1_",rep,".out"))$parameters$unstandardized[,3]
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
      SAVEDATA 	= paste0("FILE is Sim32_IgnoreTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp),
      rdata 	= coh2.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                                 modelout = paste0("Sim32_Ignore2_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')   
    
    #cohort 3 baseline
    model.Ignore3 <- paste0(" th1_1  by  y1@",est.Ignore1[1], " y2@",est.Ignore1[2], " y3@",est.Ignore1[3], " y4@",est.Ignore1[4],
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
    
    body.model.Ignore3 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore3,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_IgnoreTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh3.resp),
      rdata 	= coh3.resp)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                                 modelout = paste0("Sim32_Ignore3_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')
    
    ####################
    #     Cohort 1 Follow up
    ####################
    model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3],
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
    
    body.model.Ignore4 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore4,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_IgnoreTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh1.resp.fl),
      rdata 	= coh1.resp.fl)
    
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                 modelout = paste0("Sim32_Ignore4_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always') 
    
    # cohort 2 follow-up
    
    est.Ignore4=readModels(paste0("Sim32_Ignore4_",rep,".out"))$parameters$unstandardized[,3]
    
    model.Ignore5 <- paste0(" th1_1  by  y7@",est.Ignore4[1], " y8@",est.Ignore4[2], " y9@",est.Ignore4[3],"
                          y10@", est.Ignore4[4], " y11@",est.Ignore4[5], "  y12@", est.Ignore4[6], " ;",
                            "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore4[8],"];
                       [y7$2@",est.Ignore4[9],"];
                       [y7$3@",est.Ignore4[10],"];
	                     [y8$1@",est.Ignore4[11],"];
	                     [y8$2@",est.Ignore4[12],"];
	                     [y8$3@",est.Ignore4[13],"];
	                     [y9$1@",est.Ignore4[14],"];
	                     [y9$2@",est.Ignore4[15],"];
	                     [y9$3@",est.Ignore4[16],"];
                       [y10$1@",est.Ignore4[17],"];
                       [y10$2@",est.Ignore4[18],"];
                       [y10$3@",est.Ignore4[19],"];
	                     [y11$1@",est.Ignore4[20],"];
	                     [y11$2@",est.Ignore4[21],"];
	                     [y11$3@",est.Ignore4[22],"];
	                     [y12$1@",est.Ignore4[23],"];
	                     [y12$2@",est.Ignore4[24],"];
	                     [y12$3@",est.Ignore4[25],"];")
    body.model.Ignore5 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore5,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_IgnoreTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh2.resp.fl),
      rdata 	= coh2.resp.fl)
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore5, 
                                                 modelout = paste0("Sim32_Ignore5_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')  
    # cohort 3 follow-up
    
    
    model.Ignore6 <- paste0(" th1_1  by  y7@",est.Ignore4[1], " y8@",est.Ignore4[2], " y9@",est.Ignore4[3],"
                          y10@", est.Ignore4[4], " y11@",est.Ignore4[5], "  y12@", est.Ignore4[6], " ;",
                            "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore4[8],"];
                       [y7$2@",est.Ignore4[9],"];
                       [y7$3@",est.Ignore4[10],"];
	                     [y8$1@",est.Ignore4[11],"];
	                     [y8$2@",est.Ignore4[12],"];
	                     [y8$3@",est.Ignore4[13],"];
	                     [y9$1@",est.Ignore4[14],"];
	                     [y9$2@",est.Ignore4[15],"];
	                     [y9$3@",est.Ignore4[16],"];
                       [y10$1@",est.Ignore4[17],"];
                       [y10$2@",est.Ignore4[18],"];
                       [y10$3@",est.Ignore4[19],"];
	                     [y11$1@",est.Ignore4[20],"];
	                     [y11$2@",est.Ignore4[21],"];
	                     [y11$3@",est.Ignore4[22],"];
	                     [y12$1@",est.Ignore4[23],"];
	                     [y12$2@",est.Ignore4[24],"];
	                     [y12$3@",est.Ignore4[25],"];")
    body.model.Ignore6 <- mplusObject(
      VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
      ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
      MODEL 	= model.Ignore6,
      OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
      SAVEDATA 	= paste0("FILE is Sim32_IgnoreTrait6_",rep,".txt;
                       SAVE is fscores (100);"),
      
      PLOT = "TYPE=PLOT3",
      usevariables 	= names(coh3.resp.fl),
      rdata 	= coh3.resp.fl)
    
    fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore6, 
                                                 modelout = paste0("Sim32_Ignore6_",rep,".inp"), run = TRUE, 
                                                 hashfilename = FALSE, writeData = 'always')  
  }
  
    
    
  #########################################################
  #
  #     sim 3.3       Item design 2 + person design 1
  #
  #########################################################
    
    setwd("~/")
    setwd("F:/PsychMethod/sim33")
  
    J=9
    1.1/1.7
    2.8/1.7
    set.seed(1)
    Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
    Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
    Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
    Amat3=c(Amat1[4:6],Amat2[4:6]) 
    set.seed(100)
    Gamma1=runif(6,0.647,1.647) #0.9547661 0.9046725 1.1993224 0.7033832 1.1155493 1.1307707
    Gamma2=c(Gamma1[1:3],runif(3,0.647,1.647)) #0.9547661 0.9046725 1.1993224 1.4594026 1.0173205 1.1935586
    Gamma3=c(Gamma1[4:6],Gamma2[4:6])
    gra1=gra2=gra3=matrix(0,12,8)
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
    gra3[1:6,1]= Amat3*1.7
    gra3[7:12,2] = Amat3*1.7
    for (j in 1:6){
      gra3[j,j+2]=gra3[j+6,j+2]=Gamma3[j]*1.7
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
    grd3=rbind(-grorb[4:9,]*1.7,-grorb[4:9,]*1.7)
    
    N1=800
    N2=800
    N3=800
    N=N1+N2+N3
    reps=50
    set.seed(1)
    Theta1=rmvnorm(N1*reps,c(0,0.2),matrix(c(1,0.4,0.4,1),2,2))
    Theta2=rmvnorm(N2*reps,c(0.1,0.3),matrix(c(1,0.4,0.4,1),2,2))
    Theta3=rmvnorm(N3*reps,c(0.2,0.4),matrix(c(1,0.4,0.4,1),2,2))
    Theta.nuisance=matrix(rnorm(N*reps*6,0,1),N*reps,6)
    
    write.csv(Theta1,file='Condition33_trait_cohort1.csv')
    write.csv(Theta2,file='Condition33_trait_cohort2.csv')
    write.csv(Theta3,file='Condition33_trait_cohort3.csv')
    write.csv(Theta.nuisance,file='Condition33_trait_nuisance_cohort123.csv')

    
    cohort1.resp <- simdata(gra1,grd1,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
    
    cohort2.resp <- simdata(gra2,grd2,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
    
    cohort3.resp <- simdata(gra3,grd3,itemtype = "graded",Theta=cbind(Theta3,Theta.nuisance[80001:120000,]))
    

    
    write.csv(cohort1.resp,file='sim33_cohort1_resp.csv')
    write.csv(cohort2.resp,file='sim33_cohort2_resp.csv')
    write.csv(cohort3.resp,file='sim33_cohort3_resp.csv')
    
    cohort1.resp=read.csv(file='sim33_cohort1_resp.csv')[,-1]
    cohort2.resp=read.csv(file='sim33_cohort2_resp.csv')[,-1]
    cohort3.resp=read.csv(file='sim33_cohort3_resp.csv')[,-1]
    


    # 1. Concurrent calibration (the model does not converge under given sample size)
    N1=N2=N3=800
    for (rep in 1:reps){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6],NA,NA,NA,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12],NA,NA,NA)
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:3],NA,NA,NA,cohort2.resp[((rep-1)*N2+1):(rep*N2),4:9],NA,NA,NA,cohort2.resp[((rep-1)*N2+1):(rep*N2),10:12])
      coh3.resp=cbind((N1+N2+1):(N1+N2+N3),NA,NA,NA,cohort3.resp[((rep-1)*N3+1):(rep*N3),1:6],NA,NA,NA,cohort3.resp[((rep-1)*N3+1):(rep*N3),7:12])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      coh123.resp=rbind(coh1.resp,coh2.resp,coh3.resp)
      coh123.resp=cbind(coh123.resp,c(rep(1,N1),rep(2,N2),rep(3,N3)))
      colnames(coh123.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
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
                         th1_1 WITH th1_2;
                         
                         %c#3%

                         [th1_1*0];
                         th1_1*1;
                         [th1_2*0];
                         th1_2*1;
                         [th2_1-th2_9@0];
                         th2_1-th2_9*1;
                         th1_1 WITH th1_2;"
      
      body.model.concurrent <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;CLASSES=c(3); KNOWNCLASS=c(Phase=1-3);",
        ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.concurrent,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_ConcurrentTrait_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh123.resp),
        rdata 	= coh123.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                                   modelout = paste0("Sim33_Concurrent_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
    }
    
    
    # 2. Bayesian estimation (using informative priors)
    for (rep in 1:reps){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      
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
        SAVEDATA 	= paste0("FILE is Sim33_BayesianTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                   modelout = paste0("Sim33_Bayesian1_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
     
      est.Bayesian1=readModels(paste0("Sim33_Bayesian1_",rep,".out"))$parameters$unstandardized[,3]
      var.Bayesian1=readModels(paste0("Sim33_Bayesian1_",rep,".out"))$parameters$unstandardized[,4]^2
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
        SAVEDATA 	= paste0("FILE is Sim33_BayesianTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                   modelout = paste0("Sim33_Bayesian2_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      est.Bayesian2=readModels(paste0("Sim33_Bayesian2_",rep,".out"))$parameters$unstandardized[,3]
      var.Bayesian2=readModels(paste0("Sim33_Bayesian2_",rep,".out"))$parameters$unstandardized[,4]^2
      model.Bayesian3 <-" th1_1  by  y1-y6* (l_1-l_6);
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
      
      
      
      
      MODELPRIORS31 = paste0("l_1~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_2~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_3~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_4~ N(",est.Bayesian2[4]," ",var.Bayesian2[4],");
               l_5~ N(",est.Bayesian2[5]," ",var.Bayesian2[5],");
               l_6~ N(",est.Bayesian2[6]," ",var.Bayesian2[6],");
               
               
               t1~ N(",est.Bayesian1[70]," ",var.Bayesian1[70],");
               t2~ N(",est.Bayesian1[71]," ",var.Bayesian1[71],");
               t3~ N(",est.Bayesian1[72]," ",var.Bayesian1[72],");
               t4~ N(",est.Bayesian1[73]," ",var.Bayesian1[73],");
               t5~ N(",est.Bayesian1[74]," ",var.Bayesian1[74],");
               t6~ N(",est.Bayesian1[75]," ",var.Bayesian1[75],");
               t7~ N(",est.Bayesian1[76]," ",var.Bayesian1[76],");
               t8~ N(",est.Bayesian1[77]," ",var.Bayesian1[77],");
               t9~ N(",est.Bayesian1[78]," ",var.Bayesian1[78],");
               
               t10~ N(",est.Bayesian2[70]," ",var.Bayesian2[70],");
               t11~ N(",est.Bayesian2[71]," ",var.Bayesian2[71],");
               t12~ N(",est.Bayesian2[72]," ",var.Bayesian2[72],");
               t13~ N(",est.Bayesian2[73]," ",var.Bayesian2[73],");
               t14~ N(",est.Bayesian2[74]," ",var.Bayesian2[74],");
               t15~ N(",est.Bayesian2[75]," ",var.Bayesian2[75],");
               t16~ N(",est.Bayesian2[76]," ",var.Bayesian2[76],");
               t17~ N(",est.Bayesian2[77]," ",var.Bayesian2[77],");
               t18~ N(",est.Bayesian2[78]," ",var.Bayesian2[78],");")
      
      
      
      
      
      body.model.Bayesian3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian3,
        MODELPRIORS = MODELPRIORS31,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_BayesianTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh3.resp),
        rdata 	= coh3.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                   modelout = paste0("Sim33_Bayesian3_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      # re-estimate cohort2
      est.Bayesian3=readModels(paste0("Sim33_Bayesian3_",rep,".out"))$parameters$unstandardized[,3]
      
      model.Bayesian4 <- paste0("th1_1  by  y1*"," ","y2"," ","y3(l_1-l_3)"," ","
      y4@",est.Bayesian3[4]," ","y5@",est.Bayesian3[5]," ","y6@",est.Bayesian3[6],";
      th1_2  by  y7*"," ","y8"," ","y9(l_1-l_3)"," ","
      y10@",est.Bayesian3[4]," ","y11@",est.Bayesian3[5]," ","y12@",est.Bayesian3[6],";
                            
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
                       [y4$1@",est.Bayesian3[70]," ","y10$1@",est.Bayesian3[70],"];
                       [y4$2@",est.Bayesian3[71]," ", "y10$2@",est.Bayesian3[71],"];
                       [y4$3@",est.Bayesian3[72]," ", "y10$3@",est.Bayesian3[72],"];
                       [y5$1@",est.Bayesian3[73]," ", "y11$1@",est.Bayesian3[73],"];
                       [y5$2@",est.Bayesian3[74]," ", "y11$2@",est.Bayesian3[74],"];
                       [y5$3@",est.Bayesian3[75]," ", "y11$3@",est.Bayesian3[75],"];
                       [y6$1@",est.Bayesian3[76]," ", "y12$1@",est.Bayesian3[76],"];
                       [y6$2@",est.Bayesian3[77]," ", "y12$2@",est.Bayesian3[77],"];
                       [y6$3@",est.Bayesian3[78]," ", "y12$3@",est.Bayesian3[78],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
      
      
      body.model.Bayesian4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_BayesianTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian4, 
                                                   modelout = paste0("Sim33_Bayesian4_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      # re-estimate cohort1
      est.Bayesian4=readModels(paste0("Sim33_Bayesian4_",rep,".out"))$parameters$unstandardized[,3]
      model.Bayesian5 <- paste0("th1_1  by  y1@",est.Bayesian4[1]," ","y2@",est.Bayesian4[2]," ","y3@",est.Bayesian4[3]," ","
      y4@",est.Bayesian3[1]," ","y5@",est.Bayesian3[2]," ","y6@",est.Bayesian3[3],";
                            th1_2  by  y7@",est.Bayesian4[1]," ","y8@",est.Bayesian4[2]," ","y9@",est.Bayesian4[3]," ","
                            y10@",est.Bayesian3[1]," ","y11@",est.Bayesian3[2]," ","y12@",est.Bayesian3[3],";
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
                       
                       [y1$1@",est.Bayesian4[61]," "," y7$1@",est.Bayesian4[61],"];
                       [y1$2@",est.Bayesian4[62]," ", "y7$2@",est.Bayesian4[62],"];
                       [y1$3@",est.Bayesian4[63]," ", "y7$3@",est.Bayesian4[63],"];
                       [y2$1@",est.Bayesian4[64]," ", "y8$1@",est.Bayesian4[64],"];
                       [y2$2@",est.Bayesian4[65]," ", "y8$2@",est.Bayesian4[65],"];
                       [y2$3@",est.Bayesian4[66]," ", "y8$3@",est.Bayesian4[66],"];
                       [y3$1@",est.Bayesian4[67]," ", "y9$1@",est.Bayesian4[67],"];
                       [y3$2@",est.Bayesian4[68]," ", "y9$2@",est.Bayesian4[68],"];
                       [y3$3@",est.Bayesian4[69]," ", "y9$3@",est.Bayesian4[69],"];
                       [y4$1@",est.Bayesian3[61]," ", "y10$1@",est.Bayesian3[61],"];
                       [y4$2@",est.Bayesian3[62]," ", "y10$2@",est.Bayesian3[62],"];
                       [y4$3@",est.Bayesian3[63]," ", "y10$3@",est.Bayesian3[63],"];
                       [y5$1@",est.Bayesian3[64]," ", "y11$1@",est.Bayesian3[64],"];
                       [y5$2@",est.Bayesian3[65]," ", "y11$2@",est.Bayesian3[65],"];
                       [y5$3@",est.Bayesian3[66]," ", "y11$3@",est.Bayesian3[66],"];
                       [y6$1@",est.Bayesian3[67]," ", "y12$1@",est.Bayesian3[67],"];
                       [y6$2@",est.Bayesian3[68]," ", "y12$2@",est.Bayesian3[68],"];
                       [y6$3@",est.Bayesian3[69]," ", "y12$3@",est.Bayesian3[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
      
      
      body.model.Bayesian5 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian5,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_BayesianTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian5, 
                                                   modelout = paste0("Sim33_Bayesian5_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
    }
    
    
    
    # 3. Ignore repeated measure, fixed item parameters 
    

    for (rep in 1:reps){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
      coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),1:6])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      
      coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
      coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
      coh3.resp.fl=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),7:12])
      colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh3.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
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
        SAVEDATA 	= paste0("FILE is Sim33_IgnoreTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                                   modelout = paste0("Sim33_Ignore1_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      # cohort 2 baseline
      est.Ignore1=readModels(paste0("Sim33_Ignore1_",rep,".out"))$parameters$unstandardized[,3]
      
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
        SAVEDATA 	= paste0("FILE is Sim33_IgnoreTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                                   modelout = paste0("Sim33_Ignore2_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      est.Ignore2=readModels(paste0("Sim33_Ignore2_",rep,".out"))$parameters$unstandardized[,3]
      
      
      # cohort 3 baseline
      
      model.Ignore3 <- paste0(" th1_1  by  y1@",est.Ignore1[4], " y2@",est.Ignore1[5], " y3@",est.Ignore1[6], " 
                              y4@",est.Ignore2[4], " y5@",est.Ignore2[5], " y6@",est.Ignore2[6]," ;",
                              "[th1_1*0];
                       th1_1*1;
	                     [y1$1@",est.Ignore1[17],"];
                       [y1$2@",est.Ignore1[18],"];
                       [y1$3@",est.Ignore1[19],"];
	                     [y2$1@",est.Ignore1[20],"];
	                     [y2$2@",est.Ignore1[21],"];
	                     [y2$3@",est.Ignore1[22],"];
	                     [y3$1@",est.Ignore1[23],"];
	                     [y3$2@",est.Ignore1[24],"];
	                     [y3$3@",est.Ignore1[25],"];
                       [y4$1@",est.Ignore2[17],"];
                       [y4$2@",est.Ignore2[18],"];
                       [y4$3@",est.Ignore2[19],"];
	                     [y5$1@",est.Ignore2[20],"];
	                     [y5$2@",est.Ignore2[21],"];
	                     [y5$3@",est.Ignore2[22],"];
	                     [y6$1@",est.Ignore2[23],"];
	                     [y6$2@",est.Ignore2[24],"];
	                     [y6$3@",est.Ignore2[25],"];")
      
      body.model.Ignore3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_IgnoreTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh3.resp),
        rdata 	= coh3.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                                   modelout = paste0("Sim33_Ignore3_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      est.Ignore3=readModels(paste0("Sim33_Ignore3_",rep,".out"))$parameters$unstandardized[,3]
      
      
      ####################
      #     Follow-ups
      ####################
      
      # cohort 1 followup
      model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3], " y10@",est.Ignore1[4],
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
      
      body.model.Ignore4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_IgnoreTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp.fl),
        rdata 	= coh1.resp.fl)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                   modelout = paste0("Sim33_Ignore4_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
      # cohort 2 followup
      model.Ignore5 <- paste0(" th1_1  by  y7@",est.Ignore2[1], " y8@",est.Ignore2[2], " y9@",est.Ignore2[3], " y10@",est.Ignore2[4],
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
      
      body.model.Ignore5 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore5,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_IgnoreTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp.fl),
        rdata 	= coh2.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore5, 
                                                   modelout = paste0("Sim33_Ignore5_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
      # cohort 3 followup
      model.Ignore6 <- paste0(" th1_1  by  y7@",est.Ignore3[1], " y8@",est.Ignore3[2], " y9@",est.Ignore3[3], " y10@",est.Ignore3[4],
                              " y11@",est.Ignore3[5]," y12@",est.Ignore3[6]," ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore3[8],"];
                       [y7$2@",est.Ignore3[9],"];
                       [y7$3@",est.Ignore3[10],"];
	                     [y8$1@",est.Ignore3[11],"];
	                     [y8$2@",est.Ignore3[12],"];
	                     [y8$3@",est.Ignore3[13],"];
	                     [y9$1@",est.Ignore3[14],"];
	                     [y9$2@",est.Ignore3[15],"];
	                     [y9$3@",est.Ignore3[16],"];
	                     [y10$1@",est.Ignore3[17],"];
	                     [y10$2@",est.Ignore3[18],"];
	                     [y10$3@",est.Ignore3[19],"];
	                     [y11$1@",est.Ignore3[20],"];
	                     [y11$2@",est.Ignore3[21],"];
	                     [y11$3@",est.Ignore3[22],"];
	                     [y12$1@",est.Ignore3[23],"];
	                     [y12$2@",est.Ignore3[24],"];
	                     [y12$3@",est.Ignore3[25],"];")
      
      body.model.Ignore6 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore6,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim33_IgnoreTrait6_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh3.resp.fl),
        rdata 	= coh3.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore6, 
                                                   modelout = paste0("Sim33_Ignore6_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always') 
      
    }
    
    #########################################################
    #
    #     sim 3.4       Item design 2 + person design 2
    #
    #########################################################
    
    setwd("~/")
    setwd("F:/PsychMethod/sim34")
    
    J=9
    1.1/1.7
    2.8/1.7
    set.seed(1)
    Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
    Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
    Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
    Amat3=c(Amat1[4:6],Amat2[4:6]) 
    set.seed(100)
    Gamma1=runif(6,0.647,1.647) #0.9547661 0.9046725 1.1993224 0.7033832 1.1155493 1.1307707
    Gamma2=c(Gamma1[1:3],runif(3,0.647,1.647)) #0.9547661 0.9046725 1.1993224 1.4594026 1.0173205 1.1935586
    Gamma3=c(Gamma1[4:6],Gamma2[4:6])
    gra1=gra2=gra3=matrix(0,12,8)
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
    gra3[1:6,1]= Amat3*1.7
    gra3[7:12,2] = Amat3*1.7
    for (j in 1:6){
      gra3[j,j+2]=gra3[j+6,j+2]=Gamma3[j]*1.7
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
    grd3=rbind(-grorb[4:9,]*1.7,-grorb[4:9,]*1.7)
    
    N1=800
    N2=800
    N3=800
    N=N1+N2+N3
    reps=50
    set.seed(1)
    Theta1=rmvnorm(N1*reps,c(0,0.05),matrix(c(1,1.04,1.04,1.37),2,2))
    Theta2=rmvnorm(N2*reps,c(0.5,0.55),matrix(c(1.12,1.15,1.15,1.3),2,2))
    Theta3=rmvnorm(N3*reps,c(0.25,0.3),matrix(c(1,1.1,1.1,1.3),2,2))
    Theta.nuisance=matrix(rnorm(N*reps*6,0,1),N*reps,6)
    
    write.csv(Theta1,file='Condition34_trait_cohort1.csv')
    write.csv(Theta2,file='Condition34_trait_cohort2.csv')
    write.csv(Theta3,file='Condition34_trait_cohort3.csv')
    write.csv(Theta.nuisance,file='Condition34_trait_nuisance_cohort123.csv')
    
    
    cohort1.resp <- simdata(gra1,grd1,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
    
    cohort2.resp <- simdata(gra2,grd2,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))
    
    cohort3.resp <- simdata(gra3,grd3,itemtype = "graded",Theta=cbind(Theta3,Theta.nuisance[80001:120000,]))
    
    
    
    write.csv(cohort1.resp,file='sim34_cohort1_resp.csv')
    write.csv(cohort2.resp,file='sim34_cohort2_resp.csv')
    write.csv(cohort3.resp,file='sim34_cohort3_resp.csv')
    
    cohort1.resp=read.csv(file='sim34_cohort1_resp.csv')[,-1]
    cohort2.resp=read.csv(file='sim34_cohort2_resp.csv')[,-1]
    cohort3.resp=read.csv(file='sim34_cohort3_resp.csv')[,-1]
    
    
    
    # 1. Concurrent calibration (the model does not converge under given sample size)
    N1=N2=N3=800
    for (rep in 1:reps){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6],NA,NA,NA,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12],NA,NA,NA)
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:3],NA,NA,NA,cohort2.resp[((rep-1)*N2+1):(rep*N2),4:9],NA,NA,NA,cohort2.resp[((rep-1)*N2+1):(rep*N2),10:12])
      coh3.resp=cbind((N1+N2+1):(N1+N2+N3),NA,NA,NA,cohort3.resp[((rep-1)*N3+1):(rep*N3),1:6],NA,NA,NA,cohort3.resp[((rep-1)*N3+1):(rep*N3),7:12])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
      coh123.resp=rbind(coh1.resp,coh2.resp,coh3.resp)
      coh123.resp=cbind(coh123.resp,c(rep(1,N1),rep(2,N2),rep(3,N3)))
      colnames(coh123.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12",
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
                         th1_1 WITH th1_2;
                         
                         %c#3%

                         [th1_1*0];
                         th1_1*1;
                         [th1_2*0];
                         th1_2*1;
                         [th2_1-th2_9@0];
                         th2_1-th2_9*1;
                         th1_1 WITH th1_2;"
      
      body.model.concurrent <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;CLASSES=c(3); KNOWNCLASS=c(Phase=1-3);",
        ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.concurrent,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_ConcurrentTrait_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh123.resp),
        rdata 	= coh123.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                                   modelout = paste0("Sim34_Concurrent_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
    }
    
    
    # 2. Bayesian estimation (using informative priors)
    for (rep in 1:reps){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
      coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")
      
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
        SAVEDATA 	= paste0("FILE is Sim34_BayesianTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                                   modelout = paste0("Sim34_Bayesian1_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      est.Bayesian1=readModels(paste0("Sim34_Bayesian1_",rep,".out"))$parameters$unstandardized[,3]
      var.Bayesian1=readModels(paste0("Sim34_Bayesian1_",rep,".out"))$parameters$unstandardized[,4]^2
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
        SAVEDATA 	= paste0("FILE is Sim34_BayesianTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                                   modelout = paste0("Sim34_Bayesian2_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
      est.Bayesian2=readModels(paste0("Sim34_Bayesian2_",rep,".out"))$parameters$unstandardized[,3]
      var.Bayesian2=readModels(paste0("Sim34_Bayesian2_",rep,".out"))$parameters$unstandardized[,4]^2
      model.Bayesian3 <-" th1_1  by  y1-y6* (l_1-l_6);
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
      
      
      
      
      MODELPRIORS31 = paste0("l_1~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_2~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_3~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_4~ N(",est.Bayesian2[4]," ",var.Bayesian2[4],");
               l_5~ N(",est.Bayesian2[5]," ",var.Bayesian2[5],");
               l_6~ N(",est.Bayesian2[6]," ",var.Bayesian2[6],");
               
               
               t1~ N(",est.Bayesian1[70]," ",var.Bayesian1[70],");
               t2~ N(",est.Bayesian1[71]," ",var.Bayesian1[71],");
               t3~ N(",est.Bayesian1[72]," ",var.Bayesian1[72],");
               t4~ N(",est.Bayesian1[73]," ",var.Bayesian1[73],");
               t5~ N(",est.Bayesian1[74]," ",var.Bayesian1[74],");
               t6~ N(",est.Bayesian1[75]," ",var.Bayesian1[75],");
               t7~ N(",est.Bayesian1[76]," ",var.Bayesian1[76],");
               t8~ N(",est.Bayesian1[77]," ",var.Bayesian1[77],");
               t9~ N(",est.Bayesian1[78]," ",var.Bayesian1[78],");
               
               t10~ N(",est.Bayesian2[70]," ",var.Bayesian2[70],");
               t11~ N(",est.Bayesian2[71]," ",var.Bayesian2[71],");
               t12~ N(",est.Bayesian2[72]," ",var.Bayesian2[72],");
               t13~ N(",est.Bayesian2[73]," ",var.Bayesian2[73],");
               t14~ N(",est.Bayesian2[74]," ",var.Bayesian2[74],");
               t15~ N(",est.Bayesian2[75]," ",var.Bayesian2[75],");
               t16~ N(",est.Bayesian2[76]," ",var.Bayesian2[76],");
               t17~ N(",est.Bayesian2[77]," ",var.Bayesian2[77],");
               t18~ N(",est.Bayesian2[78]," ",var.Bayesian2[78],");")
      
      
      
      
      
      body.model.Bayesian3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian3,
        MODELPRIORS = MODELPRIORS31,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_BayesianTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh3.resp),
        rdata 	= coh3.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                                   modelout = paste0("Sim34_Bayesian3_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      # re-estimate cohort2
      est.Bayesian3=readModels(paste0("Sim34_Bayesian3_",rep,".out"))$parameters$unstandardized[,3]
      
      model.Bayesian4 <- paste0("th1_1  by  y1*"," ","y2"," ","y3(l_1-l_3)"," ","
      y4@",est.Bayesian3[4]," ","y5@",est.Bayesian3[5]," ","y6@",est.Bayesian3[6],";
      th1_2  by  y7*"," ","y8"," ","y9(l_1-l_3)"," ","
      y10@",est.Bayesian3[4]," ","y11@",est.Bayesian3[5]," ","y12@",est.Bayesian3[6],";
                            
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
                       [y4$1@",est.Bayesian3[70]," ","y10$1@",est.Bayesian3[70],"];
                       [y4$2@",est.Bayesian3[71]," ", "y10$2@",est.Bayesian3[71],"];
                       [y4$3@",est.Bayesian3[72]," ", "y10$3@",est.Bayesian3[72],"];
                       [y5$1@",est.Bayesian3[73]," ", "y11$1@",est.Bayesian3[73],"];
                       [y5$2@",est.Bayesian3[74]," ", "y11$2@",est.Bayesian3[74],"];
                       [y5$3@",est.Bayesian3[75]," ", "y11$3@",est.Bayesian3[75],"];
                       [y6$1@",est.Bayesian3[76]," ", "y12$1@",est.Bayesian3[76],"];
                       [y6$2@",est.Bayesian3[77]," ", "y12$2@",est.Bayesian3[77],"];
                       [y6$3@",est.Bayesian3[78]," ", "y12$3@",est.Bayesian3[78],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
      
      
      body.model.Bayesian4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_BayesianTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian4, 
                                                   modelout = paste0("Sim34_Bayesian4_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      # re-estimate cohort1
      est.Bayesian4=readModels(paste0("Sim34_Bayesian4_",rep,".out"))$parameters$unstandardized[,3]
      model.Bayesian5 <- paste0("th1_1  by  y1@",est.Bayesian4[1]," ","y2@",est.Bayesian4[2]," ","y3@",est.Bayesian4[3]," ","
      y4@",est.Bayesian3[1]," ","y5@",est.Bayesian3[2]," ","y6@",est.Bayesian3[3],";
                            th1_2  by  y7@",est.Bayesian4[1]," ","y8@",est.Bayesian4[2]," ","y9@",est.Bayesian4[3]," ","
                            y10@",est.Bayesian3[1]," ","y11@",est.Bayesian3[2]," ","y12@",est.Bayesian3[3],";
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
                       
                       [y1$1@",est.Bayesian4[61]," "," y7$1@",est.Bayesian4[61],"];
                       [y1$2@",est.Bayesian4[62]," ", "y7$2@",est.Bayesian4[62],"];
                       [y1$3@",est.Bayesian4[63]," ", "y7$3@",est.Bayesian4[63],"];
                       [y2$1@",est.Bayesian4[64]," ", "y8$1@",est.Bayesian4[64],"];
                       [y2$2@",est.Bayesian4[65]," ", "y8$2@",est.Bayesian4[65],"];
                       [y2$3@",est.Bayesian4[66]," ", "y8$3@",est.Bayesian4[66],"];
                       [y3$1@",est.Bayesian4[67]," ", "y9$1@",est.Bayesian4[67],"];
                       [y3$2@",est.Bayesian4[68]," ", "y9$2@",est.Bayesian4[68],"];
                       [y3$3@",est.Bayesian4[69]," ", "y9$3@",est.Bayesian4[69],"];
                       [y4$1@",est.Bayesian3[61]," ", "y10$1@",est.Bayesian3[61],"];
                       [y4$2@",est.Bayesian3[62]," ", "y10$2@",est.Bayesian3[62],"];
                       [y4$3@",est.Bayesian3[63]," ", "y10$3@",est.Bayesian3[63],"];
                       [y5$1@",est.Bayesian3[64]," ", "y11$1@",est.Bayesian3[64],"];
                       [y5$2@",est.Bayesian3[65]," ", "y11$2@",est.Bayesian3[65],"];
                       [y5$3@",est.Bayesian3[66]," ", "y11$3@",est.Bayesian3[66],"];
                       [y6$1@",est.Bayesian3[67]," ", "y12$1@",est.Bayesian3[67],"];
                       [y6$2@",est.Bayesian3[68]," ", "y12$2@",est.Bayesian3[68],"];
                       [y6$3@",est.Bayesian3[69]," ", "y12$3@",est.Bayesian3[69],"];
	               
	                     
	                     th1_1 WITH th1_2;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
      
      
      body.model.Bayesian5 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Bayesian5,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_BayesianTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian5, 
                                                   modelout = paste0("Sim34_Bayesian5_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      
    }
    
    
    
    # 3. Ignore repeated measure, fixed item parameters 
    
    
    for (rep in 1:reps){
      coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
      coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
      coh3.resp=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),1:6])
      colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      colnames(coh3.resp)=c("rid","y1","y2","y3","y4","y5","y6")
      
      coh1.resp.fl=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
      coh2.resp.fl=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
      coh3.resp.fl=cbind((N1+N2+1):(N1+N2+N3),cohort3.resp[((rep-1)*N3+1):(rep*N3),7:12])
      colnames(coh1.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh2.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
      colnames(coh3.resp.fl)=c("rid","y7","y8","y9","y10","y11","y12")
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
        SAVEDATA 	= paste0("FILE is Sim34_IgnoreTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp),
        rdata 	= coh1.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                                   modelout = paste0("Sim34_Ignore1_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      
      # cohort 2 baseline
      est.Ignore1=readModels(paste0("Sim34_Ignore1_",rep,".out"))$parameters$unstandardized[,3]
      
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
        SAVEDATA 	= paste0("FILE is Sim34_IgnoreTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp),
        rdata 	= coh2.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                                   modelout = paste0("Sim34_Ignore2_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      est.Ignore2=readModels(paste0("Sim34_Ignore2_",rep,".out"))$parameters$unstandardized[,3]
      
      
      # cohort 3 baseline
      
      model.Ignore3 <- paste0(" th1_1  by  y1@",est.Ignore1[4], " y2@",est.Ignore1[5], " y3@",est.Ignore1[6], " 
                              y4@",est.Ignore2[4], " y5@",est.Ignore2[5], " y6@",est.Ignore2[6]," ;",
                              "[th1_1*0];
                       th1_1*1;
	                     [y1$1@",est.Ignore1[17],"];
                       [y1$2@",est.Ignore1[18],"];
                       [y1$3@",est.Ignore1[19],"];
	                     [y2$1@",est.Ignore1[20],"];
	                     [y2$2@",est.Ignore1[21],"];
	                     [y2$3@",est.Ignore1[22],"];
	                     [y3$1@",est.Ignore1[23],"];
	                     [y3$2@",est.Ignore1[24],"];
	                     [y3$3@",est.Ignore1[25],"];
                       [y4$1@",est.Ignore2[17],"];
                       [y4$2@",est.Ignore2[18],"];
                       [y4$3@",est.Ignore2[19],"];
	                     [y5$1@",est.Ignore2[20],"];
	                     [y5$2@",est.Ignore2[21],"];
	                     [y5$3@",est.Ignore2[22],"];
	                     [y6$1@",est.Ignore2[23],"];
	                     [y6$2@",est.Ignore2[24],"];
	                     [y6$3@",est.Ignore2[25],"];")
      
      body.model.Ignore3 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y1-y6; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 100000; POINT=MEAN;",
        MODEL 	= model.Ignore3,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_IgnoreTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh3.resp),
        rdata 	= coh3.resp)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                                   modelout = paste0("Sim34_Ignore3_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')   
      est.Ignore3=readModels(paste0("Sim34_Ignore3_",rep,".out"))$parameters$unstandardized[,3]
      
      
      ####################
      #     Follow-ups
      ####################
      
      # cohort 1 followup
      model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore1[1], " y8@",est.Ignore1[2], " y9@",est.Ignore1[3], " y10@",est.Ignore1[4],
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
      
      body.model.Ignore4 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore4,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_IgnoreTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh1.resp.fl),
        rdata 	= coh1.resp.fl)
      
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                                   modelout = paste0("Sim34_Ignore4_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
      # cohort 2 followup
      model.Ignore5 <- paste0(" th1_1  by  y7@",est.Ignore2[1], " y8@",est.Ignore2[2], " y9@",est.Ignore2[3], " y10@",est.Ignore2[4],
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
      
      body.model.Ignore5 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore5,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_IgnoreTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh2.resp.fl),
        rdata 	= coh2.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore5, 
                                                   modelout = paste0("Sim34_Ignore5_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always')  
      
      # cohort 3 followup
      model.Ignore6 <- paste0(" th1_1  by  y7@",est.Ignore3[1], " y8@",est.Ignore3[2], " y9@",est.Ignore3[3], " y10@",est.Ignore3[4],
                              " y11@",est.Ignore3[5]," y12@",est.Ignore3[6]," ;",
                              "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y7$1@",est.Ignore3[8],"];
                       [y7$2@",est.Ignore3[9],"];
                       [y7$3@",est.Ignore3[10],"];
	                     [y8$1@",est.Ignore3[11],"];
	                     [y8$2@",est.Ignore3[12],"];
	                     [y8$3@",est.Ignore3[13],"];
	                     [y9$1@",est.Ignore3[14],"];
	                     [y9$2@",est.Ignore3[15],"];
	                     [y9$3@",est.Ignore3[16],"];
	                     [y10$1@",est.Ignore3[17],"];
	                     [y10$2@",est.Ignore3[18],"];
	                     [y10$3@",est.Ignore3[19],"];
	                     [y11$1@",est.Ignore3[20],"];
	                     [y11$2@",est.Ignore3[21],"];
	                     [y11$3@",est.Ignore3[22],"];
	                     [y12$1@",est.Ignore3[23],"];
	                     [y12$2@",est.Ignore3[24],"];
	                     [y12$3@",est.Ignore3[25],"];")
      
      body.model.Ignore6 <- mplusObject(
        VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
        ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
        MODEL 	= model.Ignore6,
        OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
        SAVEDATA 	= paste0("FILE is Sim34_IgnoreTrait6_",rep,".txt;
                       SAVE is fscores (100);"),
        
        PLOT = "TYPE=PLOT3",
        usevariables 	= names(coh3.resp.fl),
        rdata 	= coh3.resp.fl)
      
      fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore6, 
                                                   modelout = paste0("Sim34_Ignore6_",rep,".inp"), run = TRUE, 
                                                   hashfilename = FALSE, writeData = 'always') 
      
    }
    
    
  