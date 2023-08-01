# 3.2

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



# 1-dimensional
#delta=-d/a

my.grm.info=function(a.ls, d.mt, theta.ls) {
  
  c.info.mt=matrix(NA, ncol=length(a.ls)*4, nrow=length(theta.ls))
  i.info.mt=matrix(NA, ncol=length(a.ls), nrow=length(theta.ls))
  
  for (i in 1:length(a.ls)){
    a=a.ls[i]
    d=d.mt[,i]
    
    for (j in 1:length(theta.ls)){
      theta=theta.ls[j]
      
      d1=d[1]
      d2=d[2]
      d3=d[3]
      c.info=rep(NA, 4)
      
      p0=expression(1/(1+exp(a*(theta-d1))))
      p1=expression(exp(a*(theta-d1))/(1+exp(a*(theta-d1)))-
                      exp(a*(theta-d2))/(1+exp(a*(theta-d2))))
      p2=expression(exp(a*(theta-d2))/(1+exp(a*(theta-d2)))-
                      exp(a*(theta-d3))/(1+exp(a*(theta-d3))))
      p3=expression(exp(a*(theta-d3))/(1+exp(a*(theta-d3))))
      
      dk0=D(p0, 'theta')
      dk1=D(p1, 'theta')
      dk2=D(p2, 'theta')
      dk3=D(p3, 'theta')
      
      c.info[1]=eval(dk0)^2/(1/(1+exp(a*(theta-d1))))
      c.info[2]=eval(dk1)^2/(exp(a*(theta-d1))/(1+exp(a*(theta-d1)))-
                               exp(a*(theta-d2))/(1+exp(a*(theta-d2))))
      c.info[3]=eval(dk2)^2/(exp(a*(theta-d2))/(1+exp(a*(theta-d2)))-
                               exp(a*(theta-d3))/(1+exp(a*(theta-d3))))
      c.info[4]=eval(dk3)^2/(exp(a*(theta-d3))/(1+exp(a*(theta-d3))))
      
      i.info=sum(c.info)
      
      c.info.mt[j,(i*4-3):(i*4)]=c.info
      i.info.mt[j,i]=i.info
    }
  }
  return(list(category=c.info.mt, item=i.info.mt))
}



###Function ends####

theta.ls = matrix(seq(-4,4, by = .1))
a.ls=c(0.342, 0.555, 0.616)
d.mt=cbind(c(-9.323, -7.120, -2.853),
           c(-6.746, -3.779, -1.029),
           c(-5.485, -2.824, -0.608))

c.info.mt=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$category
i.info.mt=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item



theta.ls = matrix(seq(-4,4, by = .1))
a.ls=c(gra[1:6,1])
d.mt=cbind(-grd[1,]/gra[1,1],
           -grd[2,]/gra[2,1],
           -grd[3,]/gra[3,1],
           -grd[4,]/gra[4,1],
           -grd[5,]/gra[5,1],
           -grd[6,]/gra[6,1])


i.info.mt1=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt1))]

a.ls=c(gra[7:12,2])
d.mt=cbind(-grd[7,]/gra[7,2],
           -grd[8,]/gra[8,2],
           -grd[9,]/gra[9,2],
           -grd[10,]/gra[10,2],
           -grd[11,]/gra[11,2],
           -grd[12,]/gra[12,2])


i.info.mt2=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt2))]




# 3.4

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

grd


theta.ls = matrix(seq(-4,4, by = .1))
a.ls1=c(gra1[1:6,1])
d.mt1=cbind(-grd1[1,]/gra1[1,1],
           -grd1[2,]/gra1[2,1],
           -grd1[3,]/gra1[3,1],
           -grd1[4,]/gra1[4,1],
           -grd1[5,]/gra1[5,1],
           -grd1[6,]/gra1[6,1])


i.info.mt1=my.grm.info(a.ls=a.ls1, d.mt=d.mt1, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt1))]

a.ls=c(gra[7:12,2])
d.mt=cbind(-grd1[7,]/gra1[7,2],
           -grd1[8,]/gra1[8,2],
           -grd1[9,]/gra1[9,2],
           -grd1[10,]/gra1[10,2],
           -grd1[11,]/gra1[11,2],
           -grd1[12,]/gra1[12,2])


i.info.mt2=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt2))]



theta.ls = matrix(seq(-4,4, by = .1))
a.ls=c(gra[1:6,1])
d.mt=cbind(-grd[1,]/gra[1,1],
           -grd[2,]/gra[2,1],
           -grd[3,]/gra[3,1],
           -grd[4,]/gra[4,1],
           -grd[5,]/gra[5,1],
           -grd[6,]/gra[6,1])


i.info.mt1=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt1))]

a.ls=c(gra[7:12,2])
d.mt=cbind(-grd[7,]/gra[7,2],
           -grd[8,]/gra[8,2],
           -grd[9,]/gra[9,2],
           -grd[10,]/gra[10,2],
           -grd[11,]/gra[11,2],
           -grd[12,]/gra[12,2])


i.info.mt2=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt2))]



theta.ls = matrix(seq(-4,4, by = .1))
a.ls=c(gra[1:6,1])
d.mt=cbind(-grd[1,]/gra[1,1],
           -grd[2,]/gra[2,1],
           -grd[3,]/gra[3,1],
           -grd[4,]/gra[4,1],
           -grd[5,]/gra[5,1],
           -grd[6,]/gra[6,1])


i.info.mt1=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt1))]

a.ls=c(gra[7:12,2])
d.mt=cbind(-grd[7,]/gra[7,2],
           -grd[8,]/gra[8,2],
           -grd[9,]/gra[9,2],
           -grd[10,]/gra[10,2],
           -grd[11,]/gra[11,2],
           -grd[12,]/gra[12,2])


i.info.mt2=my.grm.info(a.ls=a.ls, d.mt=d.mt, theta.ls=theta.ls)$item
theta.ls[which.max(rowSums(i.info.mt2))]
