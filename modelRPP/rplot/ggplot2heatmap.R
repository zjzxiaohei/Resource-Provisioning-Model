library(ggplot2)
library(dplyr)
library(ggpubr)
library(foreach)
library(doParallel)
get_data<-function(directory, key, func){
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
Model<-'model1'
Sm<-'TMS'
HS<-c('HS1','HS5','HS10','HS100')
MS<-c('MS1.1','MS5','MS10','MS100','MS1000')
k<-'alpha_diversity'
E<-paste('E0.',as.character(seq(0,9,1)),sep = '')
E[11]<-'E1.0'
P<-paste('P0.',as.character(seq(0,9,1)),sep = '')
P[11]<-'P1.0'
E<-rev(E)
datalist<-list()
plotlist<-list()
datanames<-c()
n<-0
key<-c()
cores=detectCores()
cl<-makeCluster(cores[1]-2)
registerDoParallel(cl)
for (a in MS) {
  for (b in HS) {
    dr<-paste('/home/yxia415/yao/project data/modelKdata/model1data/TMS_',b,'_',a,sep = '')
    for (j in 1:length(P)) {
      for (i in 1:length(E)) {
        key[(j-1)*length(E)+i]<-paste(k,'_',E[i],'_',P[j],sep='')
      }
    }
    data<-foreach(i= 1:length(key),.combine = c,.export = c("%>%")) %dopar%
      {
        return(get_data(directory = dr, key = key[i]))
      }
    n<-n+1
    datalist[[n]]<-data
    datanames[n]<-paste(b,a,sep = '_')
  }
}
stopCluster(cl)
names(datalist)<-datanames


if (k=="alpha_diversity" | k=="gamma_diversity") {
  for (n in 1:length(datalist)) { 
  datalist[[n]]<-datalist[[n]]/log(150)
}
}



for (n in 1:length(datalist)) {
  df<- expand.grid(X=seq(0,100,10), Y=seq(0,100,10))
  df$Z<-datalist[[n]]
  plotlist[[n]]<-ggplot(df, aes(X, Y, fill= Z)) +  geom_tile(color="white") + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "yellow",midpoint = 0.5,limit= c(0,1),
    name= element_blank())+theme_minimal()+
    coord_fixed() +
    theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0))+
    theme(legend.position="none")+
    guides(fill = guide_colorbar(barwidth = 0.1, barheight = 10,title.position = "top", title.hjust = 1))
    
}
pic<-ggarrange(plotlist[[1]],plotlist[[2]],plotlist[[3]],plotlist[[4]],plotlist[[5]],plotlist[[6]],plotlist[[7]],plotlist[[8]],
          plotlist[[9]],plotlist[[10]],plotlist[[11]],plotlist[[12]],plotlist[[13]],plotlist[[14]],plotlist[[15]],plotlist[[16]],
          plotlist[[17]],plotlist[[18]],plotlist[[19]],plotlist[[20]],ncol = 4, nrow = 5)

