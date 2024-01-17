library(ggplot2)
library(dplyr)
library(ggpubr)
library(reshape2)
library(foreach)
library(doParallel)
get_mean<-function(directory, key, func){
  target_files<-list.files(directory, pattern = paste("[0-9]_",key,"_[A-Z]",sep = ""), full.names = T)
  target_matrix<-c()
  for (i in target_files)
  { 
    read.file<-read.delim(i,header = F)
    last_row<-read.file[nrow(read.file),] %>% unlist 
    first_row<-read.file[1,] %>% unlist 
    change<-last_row/first_row
    mean_change<-log(mean(change))
    target_matrix<-c(target_matrix,mean_change)
  }
  mean<-mean(target_matrix)
  return(mean)
}

get_sd<-function(directory, key, func){
  target_files<-list.files(directory, pattern = paste("[0-9]_",key,"_[A-Z]",sep = ""), full.names = T)
  target_matrix<-c()
  for (i in target_files)
  { 
    read.file<-read.delim(i,header = F)
    last_row<-read.file[nrow(read.file),] %>% unlist 
    first_row<-read.file[1,] %>% unlist 
    change<-last_row/first_row
    mean_change<-log(mean(change))
    target_matrix<-c(target_matrix,mean_change)
  }
  sd<-sd(target_matrix)
  return(sd)
}
data<-c()
data_mean<-c()
data_mean_1<-c()
data_sd<-c()
data_sd_1<-c()
Model<-c("model1data_k0",'model1data')
Sm<-'TMS'
HS<-c('HS1','HS5','HS10','HS100')
MS<-c('MS1.1','MS5','MS10','MS100','MS1000')
k<-'microbiome_fitness_distribution'
E<-c("E1.0_P0.0","E0.5_P0.5","E0.1_P0.9")
datalist<-list()
plotlist<-list()
my_list<-list()
datanames<-c()
key<-c()
n<-0
Max<-0




cores=detectCores()
cl<-makeCluster(18)
registerDoParallel(cl)


for (a in MS) {
  for (b in HS) {
    for (j in Model) {
      dr<-paste('/home/yxia415/yao/project data/modelKdata/',j,'/TMS_',b,'_',a,sep = '')
      for (i in 1:length(E)) {
        key[i]<-paste(k,'_',E[i],sep='')
      }
      data_mean_1<-foreach(i= 1:length(key),.combine = c,.export = c("%>%")) %dopar%
        {
          return(get_mean(directory = dr, key = key[i]))
        }
      data_mean<-c(data_mean,data_mean_1)
      data_sd<-foreach(i= 1:length(key),.combine = c,.export = c("%>%")) %dopar%
        {
          return(get_sd(directory = dr, key = key[i]))
        }
      data_sd<-c(data_sd,data_sd_1)
    }
    n<-n+1
    data_mean<-matrix(data_mean,nrow = 3)
    colnames(data_mean)<-c("No RPP","With RPP")
    rownames(data_mean)<-c("MA0*ME0","MA50*ME50","MA90*ME90")

    data<-melt(data_mean)
    data$sd<-data_sd
    datalist[[n]]<-data
    data_mean<-c()
    data_sd<-c()
    data<-c()
    datanames[n]<-paste(b,a,sep = '_')
  }
}


stopCluster(cl)
names(datalist)<-datanames
for (n in 1:length(datalist)) {
  pos<-datalist[[n]]$value+datalist[[n]]$sd+0.1
  
  plotlist[[n]]<-ggplot(datalist[[n]],aes(x=Var2,y=value,fill=Var1))+geom_bar(position = position_dodge(0.85),stat = "identity",width = 0.85)+
    geom_errorbar(ymin=datalist[[n]]$value-datalist[[n]]$sd,ymax=datalist[[n]]$value+datalist[[n]]$sd,width=0.3,linewidth=0.3,position = position_dodge(0.85))+
    labs(x="",y="")+theme(legend.title = element_blank(),axis.text.x = element_text(size = 10))+coord_cartesian(ylim = c(-2,8))
}

ggarrange(plotlist[[1]],plotlist[[2]],plotlist[[3]],plotlist[[4]],plotlist[[5]],plotlist[[6]],plotlist[[7]],plotlist[[8]],
          plotlist[[9]],plotlist[[10]],plotlist[[11]],plotlist[[12]],plotlist[[13]],plotlist[[14]],plotlist[[15]],plotlist[[16]],
          plotlist[[17]],plotlist[[18]],plotlist[[19]],plotlist[[20]],ncol = 4, nrow = 5)
