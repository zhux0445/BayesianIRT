ADNI1_1=readModels("/Users/zhux0445/Dropbox/Bayesian IRT Ruoyi 2020-2021/Code and results/ADNI_LAN/2_ADNI1_LGRM/mplus.adni1.mcmc.lgrm210104.out")$parameters$unstandardized[c(1:8,88:114),3]
ADNI2=readModels("/Users/zhux0445/Dropbox/Bayesian IRT Ruoyi 2020-2021/Code and results/ADNI_LAN/4_ADNI2_LGRM/mplus.adni2.mcmc.lgrm210119ni.out")$parameters$unstandardized[c(1:8,136:164),3]
ADNI1_2=readModels("/Users/zhux0445/Dropbox/Bayesian IRT Ruoyi 2020-2021/Code and results/ADNI_LAN/5_refitted_ADNI1/lan.adni1.210316(correct).out")$parameters$unstandardized[c(1:8,76:102),3]


common1=ADNI1_1
unique1=ADNI1_1

library(ggplot2)

itemn=c(rep("1",16),rep("3",16),rep("4",4),rep("5",4),rep("6",4),rep("7",6),rep("8",4))
estparas=c(ADNI1_1[c(1,9:15)],ADNI2[c(1,9:15)],
           ADNI1_1[c(3,23:29)],ADNI2[c(2,16:22)],
           ADNI1_1[c(4,30)],ADNI2[c(3,23)],
           ADNI1_1[c(5,31)],ADNI2[c(4,24)],
           ADNI1_1[c(6,32)],ADNI2[c(5,25)],
           ADNI1_1[c(7,33:34)],ADNI2[c(6,26:27)],
           ADNI1_1[c(8,35)],ADNI2[c(7,28)])
Method=c(rep("Stage 1",8),rep("Stage 2",8),
         rep("Stage 1",8),rep("Stage 2",8),
         rep("Stage 1",2),rep("Stage 2",2),
         rep("Stage 1",2),rep("Stage 2",2),
         rep("Stage 1",2),rep("Stage 2",2),
         rep("Stage 1",3),rep("Stage 2",3),
         rep("Stage 1",2),rep("Stage 2",2))
data=as.data.frame(cbind(itemn,estparas,Method))
data$itemn=factor(data$itemn,levels = unique(data$itemn))
data$Method=factor(data$Method,levels = unique(data$Method))
data$estparas=as.numeric(data$estparas)
p1 <-ggplot(data, aes(x=itemn, y=estparas,  shape=Method)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(3, 16))+
  labs(title=" ",
       x="Item", y = "Estimate")+ geom_hline(yintercept = 0)
p1
