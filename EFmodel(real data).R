setwd("/Users/zhux0445/Dropbox/Bayesian IRT Ruoyi 2020-2021/Code and results/ADNI_EF/3_Concurrent_Analysis")
data1=read.table("EFADNI12.lgrm.cor.dat")
data2=read.table("EFADNI12.lgrm.cor.concurrent.dat")
data1[782,]
data1[783,]
data1$phase=c(rep(1,782),rep(2,835))
data1[2,3]
data1=as.matrix(data1)
data1[which(data1==".")]=NA
data1=as.data.frame(data1)
rename=c("rid","y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18","Phase")
colnames(data1)=rename
table(data1$y1)
class(data1$rid)
data1$rid=as.integer(data1$rid)
data1$y1=as.integer(data1$y1)
data1$y2=as.integer(data1$y2)
data1$y3=as.integer(data1$y3)
data1$y4=as.integer(data1$y4)
data1$y5=as.integer(data1$y5)
data1$y6=as.integer(data1$y6)
data1$y7=as.integer(data1$y7)
data1$y8=as.integer(data1$y8)
data1$y9=as.integer(data1$y9)
data1$y10=as.integer(data1$y10)
data1$y11=as.integer(data1$y11)
data1$y12=as.integer(data1$y12)
data1$y13=as.integer(data1$y13)
data1$y14=as.integer(data1$y14)
data1$y15=as.integer(data1$y15)
data1$y16=as.integer(data1$y16)
data1$y17=as.integer(data1$y17)
data1$y18=as.integer(data1$y18)
class(data1$y18)
data1$Phase=as.numeric(data1$Phase)
class(data1$Phase)

#write.table(data1,file="EFADNI12.lgrm.cor.concurrent.dat")
# ADNI1 
#Longitudinal MCMC with correlated residual

model.mplus.ADNI1 <- " 
                       %OVERALL%
                       th1_1  by  y1-y9* (l_1-l_9);
	                     th1_2  by  y10-y18* (l_1-l_9); 
	                     th1_3  by  y5@1 y6@1 y7@1 y8@1 y9@1;
	                     th1_4  by  y14@1 y15@1 y16@1 y17@1 y18@1;
	                     th2_1  by  y1@1 y10@1;
	                     th2_2  by  y2@1 y11@1;
	                     th2_3  by  y3@1 y12@1;
	                     th2_4  by  y4@1 y13@1;
	                     th2_5  by  y5@1 y14@1;
	                     th2_6  by  y6@1 y15@1;
	                     th2_7  by  y7@1 y16@1;
	                     th2_8  by  y8@1 y17@1;
	                     th2_9  by  y9@1 y18@1;
	                   
                       [y1$1 y10$1] (t1);
                       [y1$2 y10$2] (t2);
                       [y1$3 y10$3] (t3);
                       [y1$4 y10$4] (t4);
                       [y1$5 y10$5] (t5);
                       [y1$6 y10$6] (t6);
                       [y1$7 y10$7] (t7);
                       [y1$8 y10$8] (t8);
                       [y1$9 y10$9] (t9);
	                     [y2$1 y11$1] (t10);
	                     [y2$2 y11$2] (t11);
	                     [y2$3 y11$3] (t12);
	                     [y2$4 y11$4] (t13);
	                     [y2$5 y11$5] (t14);
	                     [y2$6 y11$6] (t15);
	                     [y2$7 y11$7] (t16);
	                     [y2$8 y11$8] (t17);
	                     [y2$9 y11$9] (t18);
	                     [y3$1 y12$1] (t19);
	                     [y3$2 y12$2] (t20);
	                     [y3$3 y12$3] (t21);
	                     [y3$4 y12$4] (t22);
	                     [y3$5 y12$5] (t23);
	                     [y3$6 y12$6] (t24);
	                     [y3$7 y12$7] (t25);
	                     [y3$8 y12$8] (t26);
	                     [y3$9 y12$9] (t27);
	                     [y4$1 y13$1] (t28);
	                     [y4$2 y13$2] (t29);
	                     [y4$3 y13$3] (t30);
	                     [y4$4 y13$4] (t31);
	                     [y4$5 y13$5] (t32);
	                     [y4$6 y13$6] (t33);
	                     [y4$7 y13$7] (t34);
	                     [y4$8 y13$8] (t35);
	                     [y4$9 y13$9] (t36);
	                     [y5$1 y14$1] (t37);
	                     [y6$1 y15$1] (t38);
	                     [y7$1 y16$1] (t39);
	                     [y8$1 y17$1] (t40);
	                     [y9$1 y18$1] (t41);
	                     th1_1 WITH th1_2;
	                     th1_3 WITH th1_4;
	                     th1_1 WITH th1_3-th1_4@0;
	                     th1_2 WITH th1_3-th1_4@0;
                       th1_1 WITH th2_1-th2_9@0;
                       th1_2 WITH th2_1-th2_9@0;
                       th1_3 WITH th2_1-th2_9@0;
                       th1_4 WITH th2_1-th2_9@0;
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
                       [th1_3@0];
                       th1_3*1;
                       [th1_4@0];
                       th1_4*1;
                       [th2_1-th2_9@0];
                       th2_1-th2_9*1;
                       
                       %c#2%
                        [th1_1*0];
                       th1_1*1;
                       [th1_2*0];
                       th1_2*1;
                       [th1_3@0];
                       th1_3*1;
                       [th1_4@0];
                       th1_4*1;
                       [th2_1-th2_9@0];
                       th2_1-th2_9*1;"



body.mplus.ADNI1.cat.mcmc.t0t1 <- mplusObject(
  VARIABLE 	= "CATEGORICAL ARE y1-y18; 
IDVARIABLE = rid; CLASSES=c(2); KNOWNCLASS=c(Phase=1-2);",
  ANALYSIS 	= "TYPE=MIXTURE; estimator = BAYES; CHAINS=1; FBITER = 200000; POINT=MEAN;",
  MODEL 	= model.mplus.ADNI1,
  OUTPUT 	= "TECH1, TECH2, TECH8, TECH10;STANDARDIZED;",
  SAVEDATA 	= "FILE is EFADNI12traitconcurrent.txt;
				   SAVE is fscores (100);",
  PLOT = "TYPE=PLOT3",
  usevariables 	= names(data1),
  rdata 	=data1
)


fit.mplus.ADNI1.u1.cat.wlsmv <- mplusModeler(body.mplus.ADNI1.cat.mcmc.t0t1, 
                                             modelout = "EFADNI12.lgrm.cor.concurrent.inp", run = TRUE, 
                                             hashfilename = FALSE, writeData = 'always')   
setwd("C:/Users/zhux0445/Dropbox/Bayesian IRT Ruoyi 2020-2021/Code and results/ADNI_EF/3_Concurrent_Analysis")
library(MplusAutomation)
trait_est=readModels("efadni12.lgrm.cor.concurrent7.out")$savedata[,c("TH1_1.Mean","TH1_2.Mean")]
# change score
trait_est[,2]-trait_est[,1]
