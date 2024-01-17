library(ggplot2)
library(dplyr)
library(ggpubr)
library(reshape2)
#tidyverse
mean_data<-function(directory, key, func){
  target_files<-list.files(directory, pattern = paste("[0-9]_",key,"_[A-Z]",sep = ""), full.names = T)
  target_matrix<-c()
  for (i in target_files)
  { 
    read.file<-read.delim(i)
    last_row<-read.file[nrow(read.file),] %>% unlist 
    target_matrix<-c(target_matrix,last_row)
  }
  target_matrix<-matrix(target_matrix,nrow = 3)
  target_matrix<-rowMeans(target_matrix)
  return(target_matrix)
}

sd_data<-function(directory, key, func){
  target_files<-list.files(directory, pattern = paste("[0-9]_",key,"_[A-Z]",sep = ""), full.names = T)
  target_matrix<-c()
  for (i in target_files)
  { 
    read.file<-read.delim(i)
    last_row<-read.file[nrow(read.file),] %>% unlist 
    target_matrix<-c(target_matrix,last_row)
  }
  target_matrix<-matrix(target_matrix,nrow = 3)
  target_matrix<-apply(target_matrix,1,sd)
  return(target_matrix)
}
get_k_value<-function(directory, key, func){
  target_files<-list.files(directory, pattern = paste("[0-9]_",key,"_[A-Z]",sep = ""), full.names = T)
  target_matrix<-data.frame()
  for (i in target_files)
  { 
    read.file<-read.delim(i)
    
    last_row<-read.file[nrow(read.file),] %>% unlist %>% mean
    
    target_matrix<-rbind(target_matrix,last_row)
    
  }
  return(mean(target_matrix[[1]]))
}

data<-c()
data_mean<-c()
data_sd<-c()
k_value<-c()

Sm<-'TMS'
HS<-c('HS1','HS5','HS10','HS100')
MS<-c('MS1.1','MS5','MS10','MS100','MS1000')
k<-'bacteria_contents'
E<-c("E0.9_P0.1","E0.9_P0.9","E0.5_P0.5","E0.1_P0.1","E0.1_P0.9")
datalist<-list()
plotlist<-list()
datanames<-c()
key<-c()
n<-0
Max<-0


for (a in MS) {
  for (b in HS) {
    dr<-paste('/home/yxia415/yao/project data/modelKdata/model1data/TMS_',b,'_',a,sep = '')
      for (i in 1:length(E)) {
        key[i]<-paste(k,'_',E[i],sep='')
        data_mean<-c(data_mean,mean_data(directory = dr, key = key[i]))
      }
    for (i in 1:length(E)) {
      key[i]<-paste(k,'_',E[i],sep='')
      data_sd<-c(data_sd,sd_data(directory = dr, key = key[i]))
    }
  
    n<-n+1
    data_mean<-matrix(data_mean,nrow = 3)
    colnames(data_mean)<-c("MA10ME10","MA10ME90","MA50ME50","MA90ME10","MA90ME90")
    rownames(data_mean)<-c("Negative","Neutral","Positive")
    data<-melt(data_mean)
    data$sd<-data_sd
    data$K<-k_value
    datalist[[n]]<-data
    data_mean<-c()
    data_sd<-c()
    k_value<-c()
    data<-c()
    datanames[n]<-paste(b,a,sep = '_')
  }
}
names(datalist)<-datanames

for (n in 1:length(datalist)) {
  plotlist[[n]]<-ggplot(datalist[[n]],aes(x=Var2,y=value,fill=Var1))+geom_bar(position = position_dodge(0.85),stat = "identity",width = 0.85)+
  geom_errorbar(ymin=datalist[[n]]$value-datalist[[n]]$sd,ymax=datalist[[n]]$value+datalist[[n]]$sd,width=0.3,linewidth=0.3,position = position_dodge(0.85))+
  labs(x=names(datalist)[n],y="")+theme(legend.title = element_blank(),axis.text.x = element_text(size = 7))+coord_cartesian(ylim = c(-1e+12,6e+12))
}

ggarrange(plotlist[[1]],plotlist[[2]],plotlist[[3]],plotlist[[4]],plotlist[[5]],plotlist[[6]],plotlist[[7]],plotlist[[8]],
          plotlist[[9]],plotlist[[10]],plotlist[[11]],plotlist[[12]],plotlist[[13]],plotlist[[14]],plotlist[[15]],plotlist[[16]],
          plotlist[[17]],plotlist[[18]],plotlist[[19]],plotlist[[20]],ncol = 4, nrow = 5)
