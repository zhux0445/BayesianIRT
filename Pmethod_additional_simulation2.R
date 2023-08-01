######################################
# 1/19/2023 
# Psy method additional simulation
######################################

#############################
#
#      3-time-point design
#
#############################

#####################################################
# 
#         Sim 5.1
#
#####################################################


setwd("F:/PsychMethod/sim51")

library(mvtnorm)
library(mirt)
library(MplusAutomation)

J=12
1.1/1.7
2.8/1.7
set.seed(1)
Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
Amat3=c(Amat1[4:6],runif(3,0.647,1.647)) #0.7087863 0.8529746 0.8235568
set.seed(100)
Gamma1=runif(6,0.647,1.647) #0.9547661 0.9046725 1.1993224 0.7033832 1.1155493 1.1307707
gra=matrix(0,18,9)
gra[1:6,1]= Amat1*1.7
gra[7:12,2] = Amat2*1.7
gra[13:18,3] = Amat3*1.7
for (j in 1:3){
  gra[j,j+3]=gra[j+6,j+3]=Gamma1[j]*1.7
}

for (j in 4:6){
  gra[j,j+3]=gra[j+9,j+3]=Gamma1[j]*1.7
}

set.seed(1)
grb1<-runif(J,-3,-1)
grb2<-runif(J,-1,1)
grb3<-runif(J,1,3)


grb<-matrix(c(grb1,grb2,grb3),nrow=J)
grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
grorb
round(grorb,3)
grd=rbind(-grorb[1:6,]*1.7,-grorb[1:3,]*1.7,-grorb[7:9,]*1.7,-grorb[4:6,]*1.7,-grorb[10:12,]*1.7)

N1=800
N2=800
N=N1+N2
reps=50
set.seed(1)
Theta1=rmvnorm(N1*reps,c(0,0.2,0.3),matrix(c(1,0.4,0.4,0.4,1,0.4,0.4,0.4,1),3,3))
Theta2=rmvnorm(N2*reps,c(0.1,0.3,0.45),matrix(c(1,0.4,0.4,0.4,1,0.4,0.4,0.4,1),3,3))

Theta.nuisance=matrix(rnorm(N*reps*J,0,1),N*reps,6)

write.csv(Theta1,file='Condition51_trait_cohort1.csv')
write.csv(Theta2,file='Condition51_trait_cohort2.csv')
write.csv(Theta.nuisance,file='Condition51_trait_nuisance_cohort12.csv')


cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))

write.csv(cohort1.resp,file='sim51_cohort1_resp.csv')
write.csv(cohort2.resp,file='sim51_cohort2_resp.csv')



cohort1.resp=read.csv(file='sim51_cohort1_resp.csv')[,-1]
cohort2.resp=read.csv(file='sim51_cohort2_resp.csv')[,-1]


# 1. Concurrent calibration (provided that the model converges)
N1=N2=N3=800
for (rep in 2:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  coh12.resp=rbind(coh1.resp,coh2.resp)
  coh12.resp=cbind(coh12.resp,c(rep(1,N1),rep(2,N2)))
  colnames(coh12.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18","Phase")
  
  model.concurrent <- " %OVERALL%
                       th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th1_3  by  y13-y18* (l_4-l_6 l_10-l_12); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	         
                       
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1 y13$1] (t10);
                       [y4$2 y13$2] (t11);
                       [y4$3 y13$3] (t12);
	                     [y5$1 y14$1] (t13);
	                     [y5$2 y14$2] (t14);
	                     [y5$3 y14$3] (t15);
	                     [y6$1 y15$1] (t16);
	                     [y6$2 y15$2] (t17);
	                     [y6$3 y15$3] (t18);
	                     
	                   
	                     th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;
  
                        %c#1%
  	                     [th1_1@0];
                         th1_1 @1;
                         [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       
  	                     %c#2%
                         [th1_1*0];
                         th1_1 *1;
                         [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;"
                      
  
  body.model.concurrent <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid; CLASSES=c(2); KNOWNCLASS=c(Phase=1-2);",
    ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.concurrent,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim51_ConcurrentTrait_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh12.resp),
    rdata 	= coh12.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                               modelout = paste0("Sim51_Concurrent_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
}


# 2. Bayesian estimation (using informative priors)
for (rep in 2:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  
  model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th1_3  by  y13-y18* (l_4-l_6 l_10-l_12);
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	         
                       [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
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
	                     [y4$1 y13$1] (t10);
                       [y4$2 y13$2] (t11);
                       [y4$3 y13$3] (t12);
	                     [y5$1 y14$1] (t13);
	                     [y5$2 y14$2] (t14);
	                     [y5$3 y14$3] (t15);
	                     [y6$1 y15$1] (t16);
	                     [y6$2 y15$2] (t17);
	                     [y6$3 y15$3] (t18);
	                     
	                     th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
  
  
  body.model.Bayesian1 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian1,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim51_BayesianTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                               modelout = paste0("Sim51_Bayesian1_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Bayesian1=readModels(paste0("Sim51_Bayesian1_",rep,".out"))$parameters$unstandardized[,3]
  var.Bayesian1=readModels(paste0("Sim51_Bayesian1_",rep,".out"))$parameters$unstandardized[,4]^2
  #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
  
  model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th1_3  by  y13-y18* (l_4-l_6 l_10-l_12);
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	         
                       [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
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
	                     [y4$1 y13$1] (t10);
                       [y4$2 y13$2] (t11);
                       [y4$3 y13$3] (t12);
	                     [y5$1 y14$1] (t13);
	                     [y5$2 y14$2] (t14);
	                     [y5$3 y14$3] (t15);
	                     [y6$1 y15$1] (t16);
	                     [y6$2 y15$2] (t17);
	                     [y6$3 y15$3] (t18);
	                     [y10$1] (t19);
                       [y10$2] (t20);
                       [y10$3] (t21);
                       [y11$1] (t22);
                       [y11$2] (t23);
                       [y11$3] (t24);
                       [y12$1] (t25);
                       [y12$2] (t26);
                       [y12$3] (t27);
                       [y16$1] (t28);
                       [y16$2] (t29);
                       [y16$3] (t30);
                       [y17$1] (t31);
                       [y17$2] (t32);
                       [y17$3] (t33);
                       [y18$1] (t34);
                       [y18$2] (t35);
                       [y18$3] (t36);
	                     
	                     th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
  
  
  
  
  MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               l_4~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_5~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_6~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_7~ N(",est.Bayesian1[10]," ",var.Bayesian1[10],");
               l_8~ N(",est.Bayesian1[11]," ",var.Bayesian1[11],");
               l_9~ N(",est.Bayesian1[12]," ",var.Bayesian1[12],");
               l_10~ N(",est.Bayesian1[16]," ",var.Bayesian1[16],");
               l_11~ N(",est.Bayesian1[17]," ",var.Bayesian1[17],");
               l_12~ N(",est.Bayesian1[18]," ",var.Bayesian1[18],");
               
               t1~ N(",est.Bayesian1[76]," ",var.Bayesian1[76],");
               t2~ N(",est.Bayesian1[77]," ",var.Bayesian1[77],");
               t3~ N(",est.Bayesian1[78]," ",var.Bayesian1[78],");
               t4~ N(",est.Bayesian1[79]," ",var.Bayesian1[79],");
               t5~ N(",est.Bayesian1[80]," ",var.Bayesian1[80],");
               t6~ N(",est.Bayesian1[81]," ",var.Bayesian1[81],");
               t7~ N(",est.Bayesian1[82]," ",var.Bayesian1[82],");
               t8~ N(",est.Bayesian1[83]," ",var.Bayesian1[83],");
               t9~ N(",est.Bayesian1[84]," ",var.Bayesian1[84],");
               t10~ N(",est.Bayesian1[85]," ",var.Bayesian1[85],");
               t11~ N(",est.Bayesian1[86]," ",var.Bayesian1[86],");
               t12~ N(",est.Bayesian1[87]," ",var.Bayesian1[87],");
               t13~ N(",est.Bayesian1[88]," ",var.Bayesian1[88],");
               t14~ N(",est.Bayesian1[89]," ",var.Bayesian1[89],");
               t15~ N(",est.Bayesian1[90]," ",var.Bayesian1[90],");
               t16~ N(",est.Bayesian1[91]," ",var.Bayesian1[91],");
               t17~ N(",est.Bayesian1[92]," ",var.Bayesian1[92],");
               t18~ N(",est.Bayesian1[93]," ",var.Bayesian1[93],");
               t19~ N(",est.Bayesian1[103]," ",var.Bayesian1[103],");
               t20~ N(",est.Bayesian1[104]," ",var.Bayesian1[104],");
               t21~ N(",est.Bayesian1[105]," ",var.Bayesian1[105],");
               t22~ N(",est.Bayesian1[106]," ",var.Bayesian1[106],");
               t23~ N(",est.Bayesian1[107]," ",var.Bayesian1[107],");
               t24~ N(",est.Bayesian1[108]," ",var.Bayesian1[108],");
               t25~ N(",est.Bayesian1[109]," ",var.Bayesian1[109],");
               t26~ N(",est.Bayesian1[110]," ",var.Bayesian1[110],");
               t27~ N(",est.Bayesian1[111]," ",var.Bayesian1[111],");
               t28~ N(",est.Bayesian1[121]," ",var.Bayesian1[121],");
               t29~ N(",est.Bayesian1[122]," ",var.Bayesian1[122],");
               t30~ N(",est.Bayesian1[123]," ",var.Bayesian1[123],");
               t31~ N(",est.Bayesian1[124]," ",var.Bayesian1[124],");
               t32~ N(",est.Bayesian1[125]," ",var.Bayesian1[125],");
               t33~ N(",est.Bayesian1[126]," ",var.Bayesian1[126],");
               t34~ N(",est.Bayesian1[127]," ",var.Bayesian1[127],");
               t35~ N(",est.Bayesian1[128]," ",var.Bayesian1[128],");
               t36~ N(",est.Bayesian1[129]," ",var.Bayesian1[129],");")
  
  
  
  
  
  body.model.Bayesian2 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian2,
    MODELPRIORS = MODELPRIORS21,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim51_BayesianTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                               modelout = paste0("Sim51_Bayesian2_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  
  # re-estimate cohort 1
  
  est.Bayesian2=readModels(paste0("Sim51_Bayesian2_",rep,".out"))$parameters$unstandardized[,3]
  model.Bayesian3 <- paste0("th1_1  by  y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3]," ","y4@",est.Bayesian2[4]," 
                            y5@",est.Bayesian2[5]," ","y6@",est.Bayesian2[6],";
                            th1_2  by  y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3]," ","y10@",est.Bayesian2[10]," 
                            y11@",est.Bayesian2[11]," ","y12@",est.Bayesian2[12],";
                            th1_3  by  y13@",est.Bayesian2[4]," ","y14@",est.Bayesian2[5]," ","y15@",est.Bayesian2[6]," ","y16@",est.Bayesian2[16]," 
                            y17@",est.Bayesian2[17]," ","y18@",est.Bayesian2[18],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       
                       [y1$1@",est.Bayesian2[76]," ", "y7$1@",est.Bayesian2[76],"];
                       [y1$2@",est.Bayesian2[77]," ", "y7$2@",est.Bayesian2[77],"];
                       [y1$3@",est.Bayesian2[78]," ", "y7$3@",est.Bayesian2[78],"];
                       [y2$1@",est.Bayesian2[79]," ", "y8$1@",est.Bayesian2[79],"];
                       [y2$2@",est.Bayesian2[80]," ", "y8$2@",est.Bayesian2[80],"];
                       [y2$3@",est.Bayesian2[81]," ", "y8$3@",est.Bayesian2[81],"];
                       [y3$1@",est.Bayesian2[82]," ", "y9$1@",est.Bayesian2[82],"];
                       [y3$2@",est.Bayesian2[83]," ", "y9$2@",est.Bayesian2[83],"];
                       [y3$3@",est.Bayesian2[84]," ", "y9$3@",est.Bayesian2[84],"];
                       [y4$1@",est.Bayesian2[85]," ", "y13$1@",est.Bayesian2[85],"];
                       [y4$2@",est.Bayesian2[86]," ", "y13$2@",est.Bayesian2[86],"];
                       [y4$3@",est.Bayesian2[87]," ", "y13$3@",est.Bayesian2[87],"];
                       [y5$1@",est.Bayesian2[88]," ", "y14$1@",est.Bayesian2[88],"];
                       [y5$2@",est.Bayesian2[89]," ", "y14$2@",est.Bayesian2[89],"];
                       [y5$3@",est.Bayesian2[90]," ", "y14$3@",est.Bayesian2[90],"];
                       [y6$1@",est.Bayesian2[91]," ", "y15$1@",est.Bayesian2[91],"];
                       [y6$2@",est.Bayesian2[92]," ", "y15$2@",est.Bayesian2[92],"];
                       [y6$3@",est.Bayesian2[93]," ", "y15$3@",est.Bayesian2[93],"];
                       

	                     
	                     th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
  
  
  body.model.Bayesian3 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian3,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim51_BayesianTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                               modelout = paste0("Sim51_Bayesian3_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
}



# 3. Ignore repeated measure, fixed item parameters 

for (rep in 2:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
   
  coh1.resp.fl1=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
  coh2.resp.fl1=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
  colnames(coh1.resp.fl1)=c("rid","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp.fl1)=c("rid","y7","y8","y9","y10","y11","y12")
  
  coh1.resp.fl2=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),13:18])
  coh2.resp.fl2=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),13:18])
  colnames(coh1.resp.fl2)=c("rid","y13","y14","y15","y16","y17","y18")
  colnames(coh2.resp.fl2)=c("rid","y13","y14","y15","y16","y17","y18")
  
  
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
    SAVEDATA 	= paste0("FILE is Sim51_IgnoreTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                               modelout = paste0("Sim51_Ignore1_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Ignore1=readModels(paste0("Sim51_Ignore1_",rep,".out"))$parameters$unstandardized[,3]
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
    SAVEDATA 	= paste0("FILE is Sim51_IgnoreTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                               modelout = paste0("Sim51_Ignore2_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  ####################
  #     Cohort 1 1st Follow up
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
    SAVEDATA 	= paste0("FILE is Sim51_IgnoreTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp.fl1),
    rdata 	= coh1.resp.fl1)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                               modelout = paste0("Sim51_Ignore3_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always') 
  
  # cohort 2 1st follow-up
  
  est.Ignore3=readModels(paste0("Sim51_Ignore3_",rep,".out"))$parameters$unstandardized[,3]
  
  model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore3[1], " y8@",est.Ignore3[2], " y9@",est.Ignore3[3],"
                          y10@", est.Ignore3[4], " y11@",est.Ignore3[5], "  y12@", est.Ignore3[6], " ;",
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
  
  
  body.model.Ignore4 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore4,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim51_IgnoreTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp.fl1),
    rdata 	= coh2.resp.fl1)
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                               modelout = paste0("Sim51_Ignore4_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')  
  
  ####################
  #     Cohort 1 2nd Follow up
  ####################
  model.Ignore5 <- paste0(" th1_1  by  y13@",est.Ignore1[4], " y14@",est.Ignore1[5], " y15@",est.Ignore1[6],
                          "
                          y16 y17 y18;",
                          "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y13$1@",est.Ignore1[17],"];
                       [y13$2@",est.Ignore1[18],"];
                       [y13$3@",est.Ignore1[19],"];
	                     [y14$1@",est.Ignore1[20],"];
	                     [y14$2@",est.Ignore1[21],"];
	                     [y14$3@",est.Ignore1[22],"];
	                     [y15$1@",est.Ignore1[23],"];
	                     [y15$2@",est.Ignore1[24],"];
	                     [y15$3@",est.Ignore1[25],"];")
  
  body.model.Ignore5 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y13-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore5,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim51_IgnoreTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp.fl2),
    rdata 	= coh1.resp.fl2)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore5, 
                                               modelout = paste0("Sim51_Ignore5_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always') 
  
  # cohort 2 1st follow-up
  
  est.Ignore5=readModels(paste0("Sim51_Ignore5_",rep,".out"))$parameters$unstandardized[,3]
  
  model.Ignore6 <- paste0(" th1_1  by  y13@",est.Ignore5[1], " y14@",est.Ignore5[2], " y15@",est.Ignore5[3],"
                          y16@", est.Ignore5[4], " y17@",est.Ignore5[5], "  y18@", est.Ignore5[6], " ;",
                          "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y13$1@",est.Ignore5[8],"];
                       [y13$2@",est.Ignore5[9],"];
                       [y13$3@",est.Ignore5[10],"];
	                     [y14$1@",est.Ignore5[11],"];
	                     [y14$2@",est.Ignore5[12],"];
	                     [y14$3@",est.Ignore5[13],"];
	                     [y15$1@",est.Ignore5[14],"];
	                     [y15$2@",est.Ignore5[15],"];
	                     [y15$3@",est.Ignore5[16],"];
                       [y16$1@",est.Ignore5[17],"];
                       [y16$2@",est.Ignore5[18],"];
                       [y16$3@",est.Ignore5[19],"];
	                     [y17$1@",est.Ignore5[20],"];
	                     [y17$2@",est.Ignore5[21],"];
	                     [y17$3@",est.Ignore5[22],"];
	                     [y18$1@",est.Ignore5[23],"];
	                     [y18$2@",est.Ignore5[24],"];
	                     [y18$3@",est.Ignore5[25],"];")
  
  
  body.model.Ignore6 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y13-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore6,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim51_IgnoreTrait6_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp.fl2),
    rdata 	= coh2.resp.fl2)
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore6, 
                                               modelout = paste0("Sim51_Ignore6_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')  
  
}

#######################
#        5.2
#######################
setwd("F:/PsychMethod/sim52")

J=12
1.1/1.7
2.8/1.7
set.seed(1)
Amatcommen=runif(3,0.647,1.647) #0.9125087 1.0191239 1.2198534 
Amat1=c(Amatcommen,runif(3,0.647,1.647)) #1.5552078 0.8486819 1.5453897
Amat2=c(Amatcommen,runif(3,0.647,1.647)) #1.5916753 1.3077978 1.2761140
Amat3=c(Amat1[4:6],runif(3,0.647,1.647)) #0.7087863 0.8529746 0.8235568
set.seed(100)
Gamma1=runif(6,0.647,1.647) #0.9547661 0.9046725 1.1993224 0.7033832 1.1155493 1.1307707
gra=matrix(0,18,9)
gra[1:6,1]= Amat1*1.7
gra[7:12,2] = Amat2*1.7
gra[13:18,3] = Amat3*1.7
for (j in 1:3){
  gra[j,j+3]=gra[j+6,j+3]=Gamma1[j]*1.7
}

for (j in 4:6){
  gra[j,j+3]=gra[j+9,j+3]=Gamma1[j]*1.7
}

set.seed(1)
grb1<-runif(J,-3,-1)
grb2<-runif(J,-1,1)
grb3<-runif(J,1,3)


grb<-matrix(c(grb1,grb2,grb3),nrow=J)
grorb<-t(apply(grb,1,sort))#make sure b parameters are ordered
grorb
round(grorb,3)
grd=rbind(-grorb[1:6,]*1.7,-grorb[1:3,]*1.7,-grorb[7:9,]*1.7,-grorb[4:6,]*1.7,-grorb[10:12,]*1.7)

N1=800
N2=800
N=N1+N2
reps=50
set.seed(1)
Theta1=rmvnorm(N1*reps,c(0,0.05,0.2),matrix(c(1,0.85,0.85,0.85,1,0.85,0.85,0.85,1),3,3))
Theta2=rmvnorm(N2*reps,c(0.5,0.55,0.3),matrix(c(1,0.85,0.85,0.85,1,0.85,0.85,0.85,1),3,3))

Theta.nuisance=matrix(rnorm(N*reps*J,0,1),N*reps,6)

write.csv(Theta1,file='Condition52_trait_cohort1.csv')
write.csv(Theta2,file='Condition52_trait_cohort2.csv')
write.csv(Theta.nuisance,file='Condition52_trait_nuisance_cohort12.csv')


cohort1.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta1,Theta.nuisance[1:40000,]))
cohort2.resp <- simdata(gra,grd,itemtype = "graded",Theta=cbind(Theta2,Theta.nuisance[40001:80000,]))

write.csv(cohort1.resp,file='sim52_cohort1_resp.csv')
write.csv(cohort2.resp,file='sim52_cohort2_resp.csv')



cohort1.resp=read.csv(file='sim52_cohort1_resp.csv')[,-1]
cohort2.resp=read.csv(file='sim52_cohort2_resp.csv')[,-1]


# 1. Concurrent calibration (provided that the model converges)
N1=N2=N3=800
for (rep in 1:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  coh12.resp=rbind(coh1.resp,coh2.resp)
  coh12.resp=cbind(coh12.resp,c(rep(1,N1),rep(2,N2)))
  colnames(coh12.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18","Phase")
  
  model.concurrent <- " %OVERALL%
                       th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th1_3  by  y13-y18* (l_4-l_6 l_10-l_12); 
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	         
                       
                       [y1$1 y7$1] (t1);
                       [y1$2 y7$2] (t2);
                       [y1$3 y7$3] (t3);
	                     [y2$1 y8$1] (t4);
	                     [y2$2 y8$2] (t5);
	                     [y2$3 y8$3] (t6);
	                     [y3$1 y9$1] (t7);
	                     [y3$2 y9$2] (t8);
	                     [y3$3 y9$3] (t9);
	                     [y4$1 y13$1] (t10);
                       [y4$2 y13$2] (t11);
                       [y4$3 y13$3] (t12);
	                     [y5$1 y14$1] (t13);
	                     [y5$2 y14$2] (t14);
	                     [y5$3 y14$3] (t15);
	                     [y6$1 y15$1] (t16);
	                     [y6$2 y15$2] (t17);
	                     [y6$3 y15$3] (t18);
	                     
	                   
	                     th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;
  
                        %c#1%
  	                     [th1_1@0];
                         th1_1 @1;
                         [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       
  	                     %c#2%
                         [th1_1*0];
                         th1_1 *1;
                         [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;"
  
  
  body.model.concurrent <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid; CLASSES=c(2); KNOWNCLASS=c(Phase=1-2);",
    ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.concurrent,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim52_ConcurrentTrait_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh12.resp),
    rdata 	= coh12.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.concurrent, 
                                               modelout = paste0("Sim52_Concurrent_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
}


# 2. Bayesian estimation (using informative priors)
for (rep in 1:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18")
  
  model.Bayesian1 <- " th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th1_3  by  y13-y18* (l_4-l_6 l_10-l_12);
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	         
                       [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
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
	                     [y4$1 y13$1] (t10);
                       [y4$2 y13$2] (t11);
                       [y4$3 y13$3] (t12);
	                     [y5$1 y14$1] (t13);
	                     [y5$2 y14$2] (t14);
	                     [y5$3 y14$3] (t15);
	                     [y6$1 y15$1] (t16);
	                     [y6$2 y15$2] (t17);
	                     [y6$3 y15$3] (t18);
	                     
	                     th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
  
  
  body.model.Bayesian1 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian1,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim52_BayesianTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian1, 
                                               modelout = paste0("Sim52_Bayesian1_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Bayesian1=readModels(paste0("Sim52_Bayesian1_",rep,".out"))$parameters$unstandardized[,3]
  var.Bayesian1=readModels(paste0("Sim52_Bayesian1_",rep,".out"))$parameters$unstandardized[,4]^2
  #trait1=readModels("mplus.ADNI1.mcmc.lgrm210104.out")$savedata[,c("TH1_1.Mean","TH1_1.Standard.Deviation","TH1_2.Mean","TH1_2.Standard.Deviation")]
  
  model.Bayesian2 <-" th1_1  by  y1-y6* (l_1-l_6);
	                     th1_2  by  y7-y12* (l_1-l_3 l_7-l_9); 
	                     th1_3  by  y13-y18* (l_4-l_6 l_10-l_12);
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	         
                       [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
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
	                     [y4$1 y13$1] (t10);
                       [y4$2 y13$2] (t11);
                       [y4$3 y13$3] (t12);
	                     [y5$1 y14$1] (t13);
	                     [y5$2 y14$2] (t14);
	                     [y5$3 y14$3] (t15);
	                     [y6$1 y15$1] (t16);
	                     [y6$2 y15$2] (t17);
	                     [y6$3 y15$3] (t18);
	                     [y10$1] (t19);
                       [y10$2] (t20);
                       [y10$3] (t21);
                       [y11$1] (t22);
                       [y11$2] (t23);
                       [y11$3] (t24);
                       [y12$1] (t25);
                       [y12$2] (t26);
                       [y12$3] (t27);
                       [y16$1] (t28);
                       [y16$2] (t29);
                       [y16$3] (t30);
                       [y17$1] (t31);
                       [y17$2] (t32);
                       [y17$3] (t33);
                       [y18$1] (t34);
                       [y18$2] (t35);
                       [y18$3] (t36);
	                     
	                     th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;"
  
  
  
  
  MODELPRIORS21 = paste0("l_1~ N(",est.Bayesian1[1]," ",var.Bayesian1[1],");
               l_2~ N(",est.Bayesian1[2]," ",var.Bayesian1[2],");
               l_3~ N(",est.Bayesian1[3]," ",var.Bayesian1[3],");
               l_4~ N(",est.Bayesian1[4]," ",var.Bayesian1[4],");
               l_5~ N(",est.Bayesian1[5]," ",var.Bayesian1[5],");
               l_6~ N(",est.Bayesian1[6]," ",var.Bayesian1[6],");
               l_7~ N(",est.Bayesian1[10]," ",var.Bayesian1[10],");
               l_8~ N(",est.Bayesian1[11]," ",var.Bayesian1[11],");
               l_9~ N(",est.Bayesian1[12]," ",var.Bayesian1[12],");
               l_10~ N(",est.Bayesian1[16]," ",var.Bayesian1[16],");
               l_11~ N(",est.Bayesian1[17]," ",var.Bayesian1[17],");
               l_12~ N(",est.Bayesian1[18]," ",var.Bayesian1[18],");
               
               t1~ N(",est.Bayesian1[76]," ",var.Bayesian1[76],");
               t2~ N(",est.Bayesian1[77]," ",var.Bayesian1[77],");
               t3~ N(",est.Bayesian1[78]," ",var.Bayesian1[78],");
               t4~ N(",est.Bayesian1[79]," ",var.Bayesian1[79],");
               t5~ N(",est.Bayesian1[80]," ",var.Bayesian1[80],");
               t6~ N(",est.Bayesian1[81]," ",var.Bayesian1[81],");
               t7~ N(",est.Bayesian1[82]," ",var.Bayesian1[82],");
               t8~ N(",est.Bayesian1[83]," ",var.Bayesian1[83],");
               t9~ N(",est.Bayesian1[84]," ",var.Bayesian1[84],");
               t10~ N(",est.Bayesian1[85]," ",var.Bayesian1[85],");
               t11~ N(",est.Bayesian1[86]," ",var.Bayesian1[86],");
               t12~ N(",est.Bayesian1[87]," ",var.Bayesian1[87],");
               t13~ N(",est.Bayesian1[88]," ",var.Bayesian1[88],");
               t14~ N(",est.Bayesian1[89]," ",var.Bayesian1[89],");
               t15~ N(",est.Bayesian1[90]," ",var.Bayesian1[90],");
               t16~ N(",est.Bayesian1[91]," ",var.Bayesian1[91],");
               t17~ N(",est.Bayesian1[92]," ",var.Bayesian1[92],");
               t18~ N(",est.Bayesian1[93]," ",var.Bayesian1[93],");
               t19~ N(",est.Bayesian1[103]," ",var.Bayesian1[103],");
               t20~ N(",est.Bayesian1[104]," ",var.Bayesian1[104],");
               t21~ N(",est.Bayesian1[105]," ",var.Bayesian1[105],");
               t22~ N(",est.Bayesian1[106]," ",var.Bayesian1[106],");
               t23~ N(",est.Bayesian1[107]," ",var.Bayesian1[107],");
               t24~ N(",est.Bayesian1[108]," ",var.Bayesian1[108],");
               t25~ N(",est.Bayesian1[109]," ",var.Bayesian1[109],");
               t26~ N(",est.Bayesian1[110]," ",var.Bayesian1[110],");
               t27~ N(",est.Bayesian1[111]," ",var.Bayesian1[111],");
               t28~ N(",est.Bayesian1[121]," ",var.Bayesian1[121],");
               t29~ N(",est.Bayesian1[122]," ",var.Bayesian1[122],");
               t30~ N(",est.Bayesian1[123]," ",var.Bayesian1[123],");
               t31~ N(",est.Bayesian1[124]," ",var.Bayesian1[124],");
               t32~ N(",est.Bayesian1[125]," ",var.Bayesian1[125],");
               t33~ N(",est.Bayesian1[126]," ",var.Bayesian1[126],");
               t34~ N(",est.Bayesian1[127]," ",var.Bayesian1[127],");
               t35~ N(",est.Bayesian1[128]," ",var.Bayesian1[128],");
               t36~ N(",est.Bayesian1[129]," ",var.Bayesian1[129],");")
  
  
  
  
  
  body.model.Bayesian2 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian2,
    MODELPRIORS = MODELPRIORS21,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim52_BayesianTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian2, 
                                               modelout = paste0("Sim52_Bayesian2_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  
  # re-estimate cohort 1
  
  est.Bayesian2=readModels(paste0("Sim52_Bayesian2_",rep,".out"))$parameters$unstandardized[,3]
  model.Bayesian3 <- paste0("th1_1  by  y1@",est.Bayesian2[1]," ","y2@",est.Bayesian2[2]," ","y3@",est.Bayesian2[3]," ","y4@",est.Bayesian2[4]," 
                            y5@",est.Bayesian2[5]," ","y6@",est.Bayesian2[6],";
                            th1_2  by  y7@",est.Bayesian2[1]," ","y8@",est.Bayesian2[2]," ","y9@",est.Bayesian2[3]," ","y10@",est.Bayesian2[10]," 
                            y11@",est.Bayesian2[11]," ","y12@",est.Bayesian2[12],";
                            th1_3  by  y13@",est.Bayesian2[4]," ","y14@",est.Bayesian2[5]," ","y15@",est.Bayesian2[6]," ","y16@",est.Bayesian2[16]," 
                            y17@",est.Bayesian2[17]," ","y18@",est.Bayesian2[18],";
	                     th2_1  by  y1@1 y7@1 (n1);
	                     th2_2  by  y2@1 y8@1 (n2);
	                     th2_3  by  y3@1 y9@1 (n3);
	                     th2_4  by  y4@1 y13@1 (n4);
	                     th2_5  by  y5@1 y14@1 (n5);
	                     th2_6  by  y6@1 y15@1 (n6);
	                     [th1_1@0];
                       th1_1@1;
                       [th1_2*0];
                       th1_2*1;
                       [th1_3*0];
                       th1_3*1;
                       [th2_1-th2_6@0];
                       th2_1-th2_6*1;
                       
                       [y1$1@",est.Bayesian2[76]," ", "y7$1@",est.Bayesian2[76],"];
                       [y1$2@",est.Bayesian2[77]," ", "y7$2@",est.Bayesian2[77],"];
                       [y1$3@",est.Bayesian2[78]," ", "y7$3@",est.Bayesian2[78],"];
                       [y2$1@",est.Bayesian2[79]," ", "y8$1@",est.Bayesian2[79],"];
                       [y2$2@",est.Bayesian2[80]," ", "y8$2@",est.Bayesian2[80],"];
                       [y2$3@",est.Bayesian2[81]," ", "y8$3@",est.Bayesian2[81],"];
                       [y3$1@",est.Bayesian2[82]," ", "y9$1@",est.Bayesian2[82],"];
                       [y3$2@",est.Bayesian2[83]," ", "y9$2@",est.Bayesian2[83],"];
                       [y3$3@",est.Bayesian2[84]," ", "y9$3@",est.Bayesian2[84],"];
                       [y4$1@",est.Bayesian2[85]," ", "y13$1@",est.Bayesian2[85],"];
                       [y4$2@",est.Bayesian2[86]," ", "y13$2@",est.Bayesian2[86],"];
                       [y4$3@",est.Bayesian2[87]," ", "y13$3@",est.Bayesian2[87],"];
                       [y5$1@",est.Bayesian2[88]," ", "y14$1@",est.Bayesian2[88],"];
                       [y5$2@",est.Bayesian2[89]," ", "y14$2@",est.Bayesian2[89],"];
                       [y5$3@",est.Bayesian2[90]," ", "y14$3@",est.Bayesian2[90],"];
                       [y6$1@",est.Bayesian2[91]," ", "y15$1@",est.Bayesian2[91],"];
                       [y6$2@",est.Bayesian2[92]," ", "y15$2@",est.Bayesian2[92],"];
                       [y6$3@",est.Bayesian2[93]," ", "y15$3@",est.Bayesian2[93],"];
                       

	                     
	                     th1_1 WITH th1_2-th1_3;
                       th1_2 WITH th1_3;
                       th1_1 WITH th2_1-th2_6@0;
                       th1_2 WITH th2_1-th2_6@0;
                       th1_3 WITH th2_1-th2_6@0;
                       th2_1 WITH th2_2-th2_6@0;
                       th2_2 WITH th2_3-th2_6@0;
                       th2_3 WITH th2_4-th2_6@0;
                       th2_4 WITH th2_5-th2_6@0;
                       th2_5 WITH th2_6@0;")
  
  
  body.model.Bayesian3 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y1-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Bayesian3,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim52_BayesianTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Bayesian3, 
                                               modelout = paste0("Sim52_Bayesian3_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
}



# 3. Ignore repeated measure, fixed item parameters 

for (rep in 1:reps){
  coh1.resp=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),1:6])
  coh2.resp=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),1:6])
  colnames(coh1.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  colnames(coh2.resp)=c("rid","y1","y2","y3","y4","y5","y6")
  
  coh1.resp.fl1=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),7:12])
  coh2.resp.fl1=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),7:12])
  colnames(coh1.resp.fl1)=c("rid","y7","y8","y9","y10","y11","y12")
  colnames(coh2.resp.fl1)=c("rid","y7","y8","y9","y10","y11","y12")
  
  coh1.resp.fl2=cbind(1:N1,cohort1.resp[((rep-1)*N1+1):(rep*N1),13:18])
  coh2.resp.fl2=cbind((N1+1):(N1+N2),cohort2.resp[((rep-1)*N2+1):(rep*N2),13:18])
  colnames(coh1.resp.fl2)=c("rid","y13","y14","y15","y16","y17","y18")
  colnames(coh2.resp.fl2)=c("rid","y13","y14","y15","y16","y17","y18")
  
  
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
    SAVEDATA 	= paste0("FILE is Sim52_IgnoreTrait1_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp),
    rdata 	= coh1.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore1, 
                                               modelout = paste0("Sim52_Ignore1_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  est.Ignore1=readModels(paste0("Sim52_Ignore1_",rep,".out"))$parameters$unstandardized[,3]
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
    SAVEDATA 	= paste0("FILE is Sim52_IgnoreTrait2_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp),
    rdata 	= coh2.resp)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore2, 
                                               modelout = paste0("Sim52_Ignore2_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')   
  
  
  ####################
  #     Cohort 1 1st Follow up
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
    SAVEDATA 	= paste0("FILE is Sim52_IgnoreTrait3_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp.fl1),
    rdata 	= coh1.resp.fl1)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore3, 
                                               modelout = paste0("Sim52_Ignore3_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always') 
  
  # cohort 2 1st follow-up
  
  est.Ignore3=readModels(paste0("Sim52_Ignore3_",rep,".out"))$parameters$unstandardized[,3]
  
  model.Ignore4 <- paste0(" th1_1  by  y7@",est.Ignore3[1], " y8@",est.Ignore3[2], " y9@",est.Ignore3[3],"
                          y10@", est.Ignore3[4], " y11@",est.Ignore3[5], "  y12@", est.Ignore3[6], " ;",
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
  
  
  body.model.Ignore4 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y7-y12; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore4,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim52_IgnoreTrait4_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp.fl1),
    rdata 	= coh2.resp.fl1)
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore4, 
                                               modelout = paste0("Sim52_Ignore4_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')  
  
  ####################
  #     Cohort 1 2nd Follow up
  ####################
  model.Ignore5 <- paste0(" th1_1  by  y13@",est.Ignore1[4], " y14@",est.Ignore1[5], " y15@",est.Ignore1[6],
                          "
                          y16 y17 y18;",
                          "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y13$1@",est.Ignore1[17],"];
                       [y13$2@",est.Ignore1[18],"];
                       [y13$3@",est.Ignore1[19],"];
	                     [y14$1@",est.Ignore1[20],"];
	                     [y14$2@",est.Ignore1[21],"];
	                     [y14$3@",est.Ignore1[22],"];
	                     [y15$1@",est.Ignore1[23],"];
	                     [y15$2@",est.Ignore1[24],"];
	                     [y15$3@",est.Ignore1[25],"];")
  
  body.model.Ignore5 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y13-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore5,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim52_IgnoreTrait5_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh1.resp.fl2),
    rdata 	= coh1.resp.fl2)
  
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore5, 
                                               modelout = paste0("Sim52_Ignore5_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always') 
  
  # cohort 2 1st follow-up
  
  est.Ignore5=readModels(paste0("Sim52_Ignore5_",rep,".out"))$parameters$unstandardized[,3]
  
  model.Ignore6 <- paste0(" th1_1  by  y13@",est.Ignore5[1], " y14@",est.Ignore5[2], " y15@",est.Ignore5[3],"
                          y16@", est.Ignore5[4], " y17@",est.Ignore5[5], "  y18@", est.Ignore5[6], " ;",
                          "
                      [th1_1*0];
                       th1_1*1;
	                     
	                     [y13$1@",est.Ignore5[8],"];
                       [y13$2@",est.Ignore5[9],"];
                       [y13$3@",est.Ignore5[10],"];
	                     [y14$1@",est.Ignore5[11],"];
	                     [y14$2@",est.Ignore5[12],"];
	                     [y14$3@",est.Ignore5[13],"];
	                     [y15$1@",est.Ignore5[14],"];
	                     [y15$2@",est.Ignore5[15],"];
	                     [y15$3@",est.Ignore5[16],"];
                       [y16$1@",est.Ignore5[17],"];
                       [y16$2@",est.Ignore5[18],"];
                       [y16$3@",est.Ignore5[19],"];
	                     [y17$1@",est.Ignore5[20],"];
	                     [y17$2@",est.Ignore5[21],"];
	                     [y17$3@",est.Ignore5[22],"];
	                     [y18$1@",est.Ignore5[23],"];
	                     [y18$2@",est.Ignore5[24],"];
	                     [y18$3@",est.Ignore5[25],"];")
  
  
  body.model.Ignore6 <- mplusObject(
    VARIABLE 	= "CATEGORICAL ARE y13-y18; 
    IDVARIABLE = rid;",
    ANALYSIS 	= "TYPE=GENERAL; estimator = BAYES; CHAINS=1; FBITER = 50000; POINT=MEAN;",
    MODEL 	= model.Ignore6,
    OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
    SAVEDATA 	= paste0("FILE is Sim52_IgnoreTrait6_",rep,".txt;
                       SAVE is fscores (100);"),
    
    PLOT = "TYPE=PLOT3",
    usevariables 	= names(coh2.resp.fl2),
    rdata 	= coh2.resp.fl2)
  
  fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.model.Ignore6, 
                                               modelout = paste0("Sim52_Ignore6_",rep,".inp"), run = TRUE, 
                                               hashfilename = FALSE, writeData = 'always')  
  
}
