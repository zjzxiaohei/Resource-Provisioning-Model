library(ggplot2)
library(dplyr)
library(ggpubr)
plotlist1<-list()
plotlist2<-list()
datalist<- readRDS("C:/Users/Barry/Desktop/uoa master project/rplot_K/kdata.RDS")
rowmean<-list()
colmean<-list()
data<-c()
Model<-'model1'
Sm<-'TMS'
HS<-c('HS1','HS5','HS10','HS100')
MS<-c('MS1.1','MS5','MS10','MS100','MS1000')
k<-'k'
E<-paste('E0.',as.character(seq(0,9,1)),sep = '')
E[11]<-'E1.0'
P<-paste('P0.',as.character(seq(0,9,1)),sep = '')
P[11]<-'P1.0'
E<-rev(E)
datanames<-c()


for (a in 1:20) {
  data2<-datalist[[a]]
  data2<-matrix(data2,nrow = 11,ncol = 11)
  colmean[[a]]<-colMeans(data2)
  dataframecolmean<-data.frame(seq(0,1,0.1),colmean[[a]])
  names(dataframecolmean)<-c('PP','kk')
  modelcolmean=lm(kk~PP,data = dataframecolmean)
  p_value<-summary(modelcolmean)$coefficients[2,4]
  if (p_value<0.001) {
    p_value<-"***"
  }else if(p_value<0.01){
    p_value<-"**"
  }else if(p_value<0.05){
    p_value<-"*"
  }else{
    p_value<-""
  }
  plotlist1[[a]]<-ggplot(dataframecolmean,aes(x=PP,y=kk))+
    geom_point(color="black",size=1,shape=16)+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    #scale_y_continuous(breaks = seq(0,1,0.2))+
    ylim(-0.1,1)+
    geom_smooth(method = "lm",se =FALSE,color="red",formula = y~x,linewidth=0.75)+
    labs(title="",x="",y="")+
    stat_regline_equation(label.x = 0,label.y = 0.9,aes(label= ..eq.label..),size=4.5)+
    stat_regline_equation(label.x = 0,label.y = 0.72,aes(label= ..rr.label..),size=4.5)+
    geom_text(x=0.7,y=0.7,label=p_value,size=4)+
    theme(axis.text = element_text(size=7))
  

  
  
  
  
}
for (b in 1:20) {
  data3<-datalist[[b]]
  data3<-matrix(data3,nrow = 11,ncol = 11)
  rowmean[[b]]<-rowMeans(data3)
  dataframerowmean<-data.frame(seq(0,1,0.1),rowmean[[b]])
  names(dataframerowmean)<-c('EE','kk')
  modelrowmean=lm(kk~EE,data = dataframerowmean)
  p_value<-summary(modelrowmean)$coefficients[2,4]
  if (p_value<0.001) {
    p_value<-"***"
  }else if(p_value<0.01){
    p_value<-"**"
  }else if(p_value<0.05){
    p_value<-"*"
  }else{
    p_value<-""
  }
  plotlist2[[b]]<-ggplot(dataframerowmean,aes(x=EE,y=kk))+
    geom_point(color="black",size=1,shape=16)+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    ylim(-0.1,1)+
    geom_smooth(method = "lm",se =FALSE,color="green",formula = y~x,linewidth=0.75)+
    labs(title="",x="",y="")+
    stat_regline_equation(label.x = 0,label.y = 0.9,aes(label= ..eq.label..),size=4.5)+
    stat_regline_equation(label.x = 0,label.y = 0.72,aes(label= ..rr.label..),size=4.5)+
    geom_text(x=0.7,y=0.7,label=p_value,size=4)+
    theme(axis.text = element_text(size=7))

  
}

p1<-ggarrange(plotlist1[[1]],plotlist1[[2]],plotlist1[[3]],plotlist1[[4]],plotlist1[[5]],plotlist1[[6]],plotlist1[[7]],plotlist1[[8]],
          plotlist1[[9]],plotlist1[[10]],plotlist1[[11]],plotlist1[[12]],plotlist1[[13]],plotlist1[[14]],plotlist1[[15]],plotlist1[[16]],
          plotlist1[[17]],plotlist1[[18]],plotlist1[[19]],plotlist1[[20]],ncol = 4, nrow = 5)
p2<-ggarrange(plotlist2[[1]],plotlist2[[2]],plotlist2[[3]],plotlist2[[4]],plotlist2[[5]],plotlist2[[6]],plotlist2[[7]],plotlist2[[8]],
              plotlist2[[9]],plotlist2[[10]],plotlist2[[11]],plotlist2[[12]],plotlist2[[13]],plotlist2[[14]],plotlist2[[15]],plotlist2[[16]],
              plotlist2[[17]],plotlist2[[18]],plotlist2[[19]],plotlist2[[20]],ncol = 4, nrow = 5)
